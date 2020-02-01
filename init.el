;;;;;;;;;;;
;; TODOs ;;
;;
;; * Show git status of lines in side-bar, i.e.
;;   * Green  "+" for added    a line
;;   * Yellow "~" for modified a line
;;   * Red    "-" for removed  a line
;;
;; * Configure modeline to be something... interesting. Options:
;;   * doom-modeline
;;   * spaceline
;;   * smart-mode-line
;;   * See: http://www.holgerschurig.de/en/emacs-tayloring-the-built-in-mode-line/
;;   * Git on modeline: https://gitlab.com/mark.feller/emacs.d/blob/master/modules/module-solarized.el#L36

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

;; Provides autocomplete
(use-package company
  :ensure t
  :init (global-company-mode))

;; Extends company to show function definitions.
(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))

;; Providing posframe support to company, instead of pos-tip.
(use-package company-posframe
  :ensure t
  :hook (company-mode . company-posframe-mode))

;; Provides
(use-package dired-sidebar
  :ensure t)

;; Provides jump-to-definition
(use-package dumb-jump
  :ensure t
  :init (dumb-jump-mode))

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

;; macOS friendly alternative to pos-tip.
(use-package posframe
  :ensure t)

;; Allows for Ctrl-P like searching in projects
(use-package projectile
  :ensure t
  :init (projectile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-Specific Configuration ;;

;; Clojure
(use-package clojure-mode
  :ensure t)

(use-package cider
  :ensure t)

;; Go
(use-package go-mode
  :ensure t)

(use-package company-go
  :after company go-mode
  :ensure t)

;; Lean
(use-package lean-mode
  :ensure t)

(use-package company-lean
  :after company lean-mode
  :ensure t)

;; Markdown
(use-package markdown-mode
  :ensure t)

;; Python
(use-package python-mode
  :ensure t)

(use-package elpy
  :ensure t)

(use-package blacken
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading Configurations ;;
(add-to-list 'load-path "~/.emacs.d/elisp")

(load-library "company")
(load-library "dired-sidebar")
(load-library "evil")
(load-library "flycheck")
(load-library "fill-column-indicator-conf")
(load-library "go")
(load-library "helm")
(load-library "lean")
(load-library "python")
(load-library "scroll-conf")

;; Defining and using a minor mode to override hotkeys.
(load-library "crockeo-mode")
(global-crockeo-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc Configurations ;;

;; Indent when pressing return
(global-set-key (kbd "RET") 'newline-and-indent)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Moving all backups to a single directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Moving all autosaves to a single directory
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backup/" t)))

;; Removing emacs menu bar / tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Adding line numbers to the left
(global-display-line-numbers-mode)

(defvar display-line-numbers-width-start)
(setq display-line-numbers-width-start 1)

;; Setting tabs to spaces
(setq-default indent-tabs-mode nil)

;; Viewing (row,col) in while editing.
(column-number-mode 1)

;; Turning of Eldoc, because it spawns new buffers
(global-eldoc-mode -1)

;; Turning off bell sound...because it sucks
(setq ring-bell-function 'ignore)

;; Making the initial buffer *scratch*, instead of *GNU Emacs*, so that
;; crockeo-mode is enabled by default.
(setq initial-buffer-choice t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Configurations ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (dumb-jump fill-column-indicator cider company-go go-mode jedi-company company-quickhelp dired-sidebar company-lean company flychecker projectile evil-nerd-commenter lean-mode markdown-mode use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
