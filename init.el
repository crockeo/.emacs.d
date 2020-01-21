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

;; Provides vim-like experience
(use-package evil
  :ensure t
  :init (evil-mode))

;; Adds multi-line commenting
(use-package evil-nerd-commenter
  :ensure t)

;; Adds live syntax checking
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Allows for fuzzy search
(use-package helm
  :ensure t
  :init (helm-mode))

;; Speeds up helm
(use-package helm-ag
  :after helm
  :ensure t)

;; Makes helm + projectile play nice
(use-package helm-projectile
  :after helm projectile
  :ensure t)

;; Makes emacs pretty
(use-package monokai-theme
  :ensure t)

;; Allows for Ctrl-P like searching in projects
(use-package projectile
  :ensure t
  :init (projectile-mode))

;; Language-specific packages
(use-package markdown-mode :ensure t) ; Markdown
(use-package lean-mode :ensure t) ; Lean
(use-package python-mode :ensure t) ; Python

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading Configurations ;;
(add-to-list 'load-path "~/.emacs.d/elisp/")

(load-library "evil") ; evil-mode config
(load-library "helm") ; helm-mode config
(load-library "hotkeys") ; Hotkeys used across emacsa
(load-library "projectile") ; projectile-mode config

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
    (flychecker projectile evil-nerd-commenter lean-mode markdown-mode use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
