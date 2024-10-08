(scroll-bar-mode -1) 
(menu-bar-mode -1) 
(tool-bar-mode -1) 
(setq inhibit-startup-screen 1)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(use-package evil
  :config
  (evil-mode))

(use-package ido
  :init
  (ido-mode))

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package org-xournalpp
  :ensure t
  :config
  (add-hook 'org-mode-hook 'org-xournalpp-mode))

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(custom-safe-themes
   '("72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" "3e374bb5eb46eb59dbd92578cae54b16de138bc2e8a31a2451bf6fdb0f3fd81b" "fa49766f2acb82e0097e7512ae4a1d6f4af4d6f4655a48170d0a00bcb7183970" "d80952c58cf1b06d936b1392c38230b74ae1a2a6729594770762dc0779ac66b7" "2ff9ac386eac4dffd77a33e93b0c8236bb376c5a5df62e36d4bfa821d56e4e20" "f56e81765ccd0ee403860bd1d0a2f9967aa132b4a6f40517dd5eb13f7726eaba" "125744c0b04c7addbe25d238a4053740fad2ad5e18211662f637dbacb3802331" default))
 '(org-agenda-files '("/home/admin/my_notes/gate_cse/toc/notes.org"))
 '(package-selected-packages
   '(yasnippet-snippets yasnippet company-math openwith grey-paper-theme gruvbox-theme company company-c-headers company-ctags helm-lsp lsp-haskell lsp-ivy lsp-ui lsp-mode lsp-treemacs white-theme dap-mode which-key f dash s org-xournalpp smex quelpa-use-package evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(require 'openwith)
(openwith-mode t)
(setq openwith-associations '(("\\.pdf\\'" "zathura" (file))))
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-math-symbols-latex)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(require 'yasnippet)
(yas-global-mode 1)
