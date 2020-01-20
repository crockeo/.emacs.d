;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up Package Management ;;
(require 'package)
 
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;
;; Package List ;;
(use-package evil :ensure t) ; Provides a vim-like interface
(use-package evil-nerd-commenter :ensure t) ; Provides NerdCommenter support for Emacs
(use-package helm :ensure t) ; Provides Ctrl-P like interface
(use-package monokai-theme :ensure t) ; Provides the monokai theme (from TextMate)

;; Language-specific packages
(use-package markdown-mode :ensure t) ; Markdown
(use-package lean-mode :ensure t) ; Lean
;; TODO: Find and install language-specific packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading Configurations ;;
(add-to-list 'load-path "~/.emacs.d/elisp/")

(load-library "evil") ; evil-mode config
(load-library "helm") ; helm-mode config
(load-library "hotkeys") ; Hotkeys used across emacs

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Configurations ;;

;; Moving all backups to a single directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Removing emacs menu bar / tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Adding line numbers to the left
(global-display-line-numbers-mode)

;; Setting tabs to spaces
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Configurations ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-nerd-commenter lean-mode markdown-mode use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
