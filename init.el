;;;;;;;;;;;
;; TODOs ;;
;;
;; * Move everything to its own files. Have init.el just load-library a bunch of other things.
;;
;; * Make jedi put its in-function argument documentation into a posframe
;;   instead of the pos-tip implementation thing.

;; HEY, here's a reminder. Is Emacs running slowly? Do you want to fix that?
;;
;; * Start a profiler: M-x profiler-start
;; * See the results:  M-x profiler-results

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading Configurations ;;
(add-to-list 'load-path "~/.emacs.d/elisp")

(load-library "company")
(load-library "dired-sidebar")
(load-library "display-line-number-conf")
(load-library "doom-modeline-conf")
(load-library "eros-conf")
(load-library "evil")
(load-library "flycheck")
(load-library "git-gutter-fringe-conf")
(load-library "helm")
(load-library "scroll-conf")

;; Language configuration
(load-library "clojure-conf")
(load-library "go-conf")
(load-library "lean-conf")
(load-library "python-conf")
(load-library "markdown-conf")

;; Ya boy's custom emacs good good
(load-library "format")

(load-library "highlight-todo")
(global-highlight-todo-mode 1)

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

;; Making Emacs GC more kindly
(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer
 3
 t
 (lambda () (garbage-collect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Configurations ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (eros dumb-jump fill-column-indicator cider company-go go-mode jedi-company company-quickhelp dired-sidebar company-lean company flychecker projectile evil-nerd-commenter lean-mode markdown-mode use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
