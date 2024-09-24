;;; org-xournalpp.el --- Xournalpp support for Org Mode -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Valentin Herrmann

;; Author: Valentin Herrmann <herr.valenti.mann@gmail.com>
;; Version: 0.1.1
;; Package-Requires: ((emacs "26") (f "0.20.0") (s "1.12.0") (cl-lib "0.6") (org "9.3"))
;; URL:

;;; Commentary:

;; Xournalpp support for Org Mode
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'filenotify)
(require 'f)
(require 's)
(require 'cl-lib)
(require 'org)
(require 'org-element)

(org-link-set-parameters "xournalpp" :follow #'org-xournalpp--edit :export #'org-xournalpp--export)

(defgroup org-xournalpp nil
  "Org-xournalpp customization."
  :group 'org
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-append-ext-xopp t
  "Append automatically .xopp extension."
  :group 'org-xournalpp
  :type 'boolean
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-image-type 'svg
  "Image type of cache files."
  :group 'org-xournalpp
  :type `(choice (const :tag "svg" svg)
                 (const :tag "png" png))
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-get-new-filepath (lambda () (read-file-name "New xournalpp file: "))
  "Function returning filepath of new created image."
  :group 'org-xournalpp
  :type 'function
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-get-new-desc (lambda () (read-string "Description: "))
  "Function returning description of new created image."
  :group 'org-xournalpp
  :type 'function
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-template-getter (lambda () (org-xournalpp--resource "template.xopp"))
  "Function returning the path of the template."
  :group 'org-xournalpp
  :type 'function
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-ask-for-file-name t
  "If org-xournalpp should ask for a file-name when inserting a file.
If this is nil, a file/directory is created in the current directory.
This file is hidden, if org-xournalpp-hide-file is true.

The other behavior can be achieved by using the prefix argument."
  :group 'org-xournalpp
  :type 'boolean
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-path-default "org-xournalpp/sketch"
  "Base name to use, when creating a file for the xournal."
  :group 'org-xournalpp
  :type 'string
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-executable "xournalpp"
  "The command of org-xournalpp."
  :type 'string
  :safe #'stringp
  :group 'org-xournalpp
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-hide-file nil
  "See `org-xournalpp-ask-for-file-name'."
  :group 'org-xournalpp
  :type 'boolean
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-cache-directory-alist nil
  "See `backup-directory-alist'.
If this is nil, the cache files are saved in the current directory."
  :group 'org-xournalpp
  :type '(repeat (cons (regexp :tag "Regexp matching filename")
                       (directory :tag "Org xournalpp cache directory name")))
  :package-version '(org-xournalpp . "0.1.0"))

(defcustom org-xournalpp-export-dir "images/"
  "The path, where exported images are going to be saved."
  :group 'org-xournalpp
  :type 'string
  :package-version '(org-xournalpp . "0.1.1"))

(defcustom org-xournalpp-export-overwrite? 'ask
  "Should org-xournalpp overwrite existing image files (like .png/.svg)?

Possible values are t, nil and 'ask.
You probably want this var to have the value t, but be
aware, that this might overwrite your own files."
  :group 'org-xournalpp
  :type '(choice (const :tag "Overwrite?" t)
                 (const :tag "Skip file?" nil)
                 (const :tag "Ask?" ask))
  :package-version '(org-xournalpp . "0.1.1"))

;;; buffer local vars

(defvar-local org-xournalpp--watchers nil
  "A-list mapping file names to change watcher descriptors.")

(defvar-local org-xournalpp--overlays nil
  "A-list mapping links to overlay.")

;;; constants

(defconst org-xournalpp--dir (file-name-directory load-file-name)
  "Base directory for package.")

;;; logic

(defun org-xournalpp--message-error (msg &rest args)
  "Message MSG."
  (apply #'message (concat "org-xournalpp: " msg) args))

(defun org-xournalpp--make-cached-image-file-name (xopp-path ext)
  "Create the cached image file name for XOPP-PATH with EXT.
Normally this is the file's name with \".\" prepended
 and the swapped extension.

A match for FILE is sought in `org-xournalpp-cache-directory-alist'
\(see the documentation of that variable for details\). If the
directory for the backup doesn't exist, it is created."
  (if org-xournalpp-cache-directory-alist
      (let* ((backup-directory-alist org-xournalpp-cache-directory-alist)
             (name (make-backup-file-name-1 xopp-path)))
        (f-join (f-dirname name)
                (concat (f-base name)
                        "." ext)))
    (f-join (f-dirname xopp-path)
            (concat "." (f-base xopp-path)
                    "." ext))))

(defun org-xournalpp--resource (file)
  "Return full path of a resource FILE."
  (expand-file-name file (file-name-as-directory (concat org-xournalpp--dir "resources"))))


(defun org-xournalpp--export (xopp-path desc backend)
  "Export xournalpp canvas XOPP-PATH from Org files.
Argument DESC refers to link description.
Argument BACKEND refers to export backend."
  (let ((image-path (f-expand
                     (f-join org-xournalpp-export-dir
                             (f-swap-ext (f-filename xopp-path)
                                         (symbol-name org-xournalpp-image-type))))))
    (cl-case backend
      (html (format "<img src=\"%s\">"
                    (prog1 image-path
                      (org-xournalpp--save-image xopp-path image-path))))
      (ascii (format "%s (%s)" (or desc xopp-path) xopp-path))
      (latex (format "\\includegraphics[width=\\textheight,height=\\textwidth,keepaspectratio]{%s}"
                     (prog1 image-path
                       (org-xournalpp--save-image xopp-path image-path)))))))

(defun org-xournalpp--save-image (xopp-path image-path)
  "Extract from XOPP-PATH a .svg/.png and write it to IMAGE-PATH."
  (mkdir (f-dirname image-path) t)
  (condition-case err
      (f-copy (org-xournalpp--get-image xopp-path t)
              image-path)
    (file-already-exists
     (when (or (and (eq 'ask org-xournalpp-export-overwrite?)
                    (y-or-n-p (format "Overwrite the file \"%s\"?" image-path)))
               org-xournalpp-export-overwrite?)
       (delete-file image-path)
       (org-xournalpp--save-image xopp-path image-path)))))

(defun org-xournalpp--make-new-image (output-xopp-path template)
  "Create a new image based on a TEMPLATE at OUTPUT-XOPP-PATH."
  ;; TODO: Change image width and height based on provided argument
  (mkdir (f-dirname output-xopp-path) t)
  (f-copy template output-xopp-path))

(defun org-xournalpp--get-links ()
  "Get all xournalpp links from the current buffer."
  (org-element-map (org-element-parse-buffer) 'link
    (lambda (link)
      (when (string-equal (org-element-property :type link) "xournalpp")
        link))))

(defun org-xournalpp--event-file-path (event)
  "Get the file path from EVENT."
  (if (eq (nth 1 event) 'renamed)
      (nth 3 event)
    (nth 2 event)))

(let ((last-time-callback (current-time)))
  (defun org-xournalpp--watcher-callback (event)
    "Callback that runs after xournalpp files are modified."
    ;; only execute the callback, if the last change happened a long time (> 0.1s) earlier.
    (let ((cur-time (current-time)))
      (when (> (time-to-seconds (time-subtract cur-time
                                               last-time-callback))
               0.1)
        (setq last-time-callback cur-time)

        (let* ((xopp-path (org-xournalpp--event-file-path event))
               (links (org-xournalpp--get-links))
               (paths (mapcar (lambda (it) (expand-file-name (org-element-property :path it))) links))
               (idx (cl-position xopp-path paths :test #'string-equal)))
          (when idx (org-xournalpp--show-link (nth idx links) t)))))))

(defun org-xournalpp--add-watcher (xopp-path)
  "Setup auto-refreshing watcher for XOPP-PATH."
  (let ((desc (file-notify-add-watch xopp-path '(change) #'org-xournalpp--watcher-callback)))
    (unless (alist-get xopp-path org-xournalpp--watchers nil nil #'string-equal)
      (push (cons xopp-path desc) org-xournalpp--watchers))))

(defun org-xournalpp--edit (path &optional _)
  "Edit PATH in xournalpp."
  (let ((xopp-path (expand-file-name path)))
    (when (f-exists-p xopp-path)
      (call-process org-xournalpp-executable nil 0 nil xopp-path)
      (org-xournalpp--add-watcher xopp-path))))

(defun org-xournalpp--hide-link (link)
  "Hide LINK."
  (let ((overlay (alist-get link org-xournalpp--overlays nil nil #'eq)))
    (when overlay
      (delete-overlay overlay))))

(defun org-xournalpp--get-width-attr (link)
  "Get the width from #+ATTR_* around (before?) LINK."
  (pcase (org-element-lineage link '(paragraph))
    (`nil nil)
    (p
     (let* ((case-fold-search t)
            (end (org-element-property :post-affiliated p))
            (re "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"))
       (when (org-with-point-at
                 (org-element-property :begin p)
               (re-search-forward re end t))
         (string-to-number (match-string 1)))))))

(defun org-xournalpp--get-image-width (link)
  "Get the image width of the image at LINK."
  (cond
   ((eq org-image-actual-width t) nil)
   ((listp org-image-actual-width)
    (or
     ;; First try to find a width among
     ;; attributes associated to the paragraph
     ;; containing link.
     (org-xournalpp--get-width-attr link)
     ;; Otherwise, fall-back to provided number.
     (car org-image-actual-width)))
   ((numberp org-image-actual-width)
    org-image-actual-width)
   ((eq org-image-actual-width nil)
    (or (org-xournalpp--get-width-attr link)
        nil))
   (t nil)))

(defun org-xournalpp--get-image (xopp-path refresh)
  "Extract svg/png from given XOPP-PATH and return image data.

Regenerate the cached inline image, if REFRESH is true."
  (unless (file-remote-p xopp-path)
    (let ((cache-file (org-xournalpp--make-cached-image-file-name xopp-path (symbol-name org-xournalpp-image-type))))
      (unless (and (f-exists? cache-file)
                   (not refresh))
        (call-process org-xournalpp-executable nil 0 nil "-i" cache-file (expand-file-name xopp-path))
        (while (not (f-exists? cache-file))
          (sleep-for 0.05))) ; FIXME (why?)
      cache-file)))

(defun org-xournalpp--create-image (link refresh)
  "Extract svg/png from given LINK and return image.

Regenerate the cached inline image, if REFRESH is true.

If the path from LINK does not exist, nil is returned."
  (let ((width (org-xournalpp--get-image-width link))
        (xopp-path (f-expand (org-element-property :path link))))
    (when (f-exists? xopp-path)
        (if width
            (create-image (org-xournalpp--get-image xopp-path refresh)
                          org-xournalpp-image-type
                          nil
                          :width width)
          (create-image (org-xournalpp--get-image xopp-path refresh)
                        org-xournalpp-image-type
                        nil)))))

(defun org-xournalpp--show-link (link refresh)
  "Show the image at LINK.

Regenerate the cached inline image, if REFRESH is true."
  (org-xournalpp--hide-link link)
  (let ((image (org-xournalpp--create-image link refresh)))
    (if (not image)
        (org-xournalpp--message-error "The image with path \"%s\" cannot be displayed" (org-element-property :path link))
      (when refresh
        (image-flush image))
      (let ((ov (make-overlay
                 (org-element-property :begin link)
                 (progn
                   (goto-char (org-element-property :end link))
                   (skip-chars-backward " \t")
                   (point)))))
        (overlay-put ov 'display image)
        (overlay-put ov 'face 'default)
        (overlay-put ov 'org-image-overlay t)
        (overlay-put ov 'modification-hooks
                     (list 'org-display-inline-remove-overlay))
        (push (cons link ov) org-xournalpp--overlays)))))

(defun org-xournalpp--uniquify-file (file)
  "Count n up, til FILE is unique."
  (letrec ((f-ext-with-. (lambda (f)
                           (let ((ext (f-ext f)))
                             (if ext
                                 (s-concat "." ext)
                               ""))))
           (recur (lambda (n)
                    (let ((f (f-join (f-dirname file)
                                     (s-concat (f-base file)
                                               (number-to-string n)
                                               (funcall f-ext-with-. file)))))
                      (if (f-exists? f)
                          (funcall recur (+ 1 n))
                        f)))))
    (funcall recur 1)))

(defun org-xournalpp--generate-new-xopp-file (current-dir)
  "Create new xopp-path in CURRENT-DIR."
  (org-xournalpp--uniquify-file
   (f-join current-dir (s-concat (if org-xournalpp-hide-file "." "")
                                 org-xournalpp-path-default
                                 ".xopp"))))

(defun org-xournalpp--hide-all ()
  "Hide all xournalpp links."
  (dolist (link-ov org-xournalpp--overlays)
    (delete-overlay (cdr link-ov)))
  (setq org-xournalpp--overlays nil))

(defun org-xournalpp--enable ()
  "Enable the org-xournalpp minor mode."
  (dolist (link (org-xournalpp--get-links))
    (org-xournalpp--show-link link nil)))

(defun org-xournalpp--disable ()
  "Disable watchers and hide xournalpp images."
  (dolist (watcher org-xournalpp--watchers)
    (file-notify-rm-watch (cdr watcher)))
  (setq org-xournalpp--watchers nil)
  (org-xournalpp--hide-all))

(defun org-xournalpp--validate-path (path)
  "Validate the file PATH as a xournalpp path."
  (if (f-ext-p path "xopp")
      path
    (if org-xournalpp-append-ext-xopp
        (concat path ".xopp")
      path)))

;;;###autoload
(defun org-xournalpp-insert-new-image (output-xopp-path desc template)
  "Insert new image with description DESC and path OUTPUT-XOPP-PATH in current buffer.

With a prefix argument the state of org-xournalpp-ask-for-file-name is toggled for this function call."
  (interactive
   (let ((output-xopp-path
          (if (xor current-prefix-arg org-xournalpp-ask-for-file-name)
              (org-xournalpp--validate-path (funcall org-xournalpp-get-new-filepath))
            (org-xournalpp--generate-new-xopp-file default-directory)))
         (desc (funcall org-xournalpp-get-new-desc))
         (template (funcall org-xournalpp-template-getter)))
     (list output-xopp-path desc template)))
  (org-xournalpp--make-new-image output-xopp-path template)
  (org-insert-link nil (concat "xournalpp:" (f-relative output-xopp-path)) desc)
  ;; TODO: Enable only the new image
  (org-xournalpp--enable))

;;;###autoload
(define-minor-mode org-xournalpp-mode
  "Mode for displaying editable xournalpp images within Org file."
  :init-value nil
  (if org-xournalpp-mode (org-xournalpp--enable) (org-xournalpp--disable)))

(provide 'org-xournalpp)

;;; org-xournalpp.el ends here
