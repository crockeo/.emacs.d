;;; init.el --- Root configuration file for @crockeo's Emacs config

;;; Code:

;;
;; package configuration
;;
(defvar bootstrap-version)
(defvar straight-use-package-by-default t)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(add-to-list 'load-path "~/.emacs.d/local-pkgs")

;;
;; builtin configuration
;;
(add-hook
 'org-mode-hook
 (lambda ()
   (set-fill-column 100)
   (auto-fill-mode)))

(column-number-mode t)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode t)
(tool-bar-mode -1)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directories-alist `((".*" . ,temporary-file-directory)))
(setq exec-path (append exec-path '("/usr/local/bin" "~/bin")))
(setq flymake-no-changes-timeout 5)
(setq gc-cons-threshold (* 20 1024 1024))
(setq initial-buffer-choice "~/home.org")
(setq-default mode-line-format "    %* %f    (%c,%l)    [%m]")
(setq read-process-output-max (* 200 1024 1024))
(setq ring-bell-function 'ignore)

;; for macOS, you need to set the PATH in addition to exec-path to find certain executables
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:~/bin"))

;;
;; personal packages
;;
(require 'hawaii-theme)
(load-theme 'hawaii t)
(require 'winpop)

;;
;; editor configuration
;;
(use-package ag)

(use-package company
  :config
  (setq company-idle-delay 0.2)
  (setq company-minimum-prefix-length 1)
  :hook (lsp-mode . (lambda () (company-mode 1) (push 'company-capf company-backends))))

(use-package company-prescient
  :after (company prescient))

(use-package counsel-projectile
  :after (ivy projectile))

(use-package dired-sidebar
  :config
  (add-hook 'dired-sidebar-mode
	    (lambda () (message "hello world") (display-line-numbers-mode nil))))

(use-package eros
  :config (eros-mode 1))

(use-package evil
  :config
  (evil-mode 1)
  
  (evil-define-key nil evil-insert-state-map
    "\C-a" 'evil-beginning-of-line
    "\C-e" (lambda () (interactive)
	     (evil-end-of-line)
	     (unless (equal (buffer-substring
			     (line-beginning-position) (line-end-position))
			    "")
	       (forward-char)))
    "\C-f" 'evil-normal-state)

  (evil-define-key nil evil-normal-state-map
    "u" 'undo-fu-only-undo
    "\C-a" 'evil-beginning-of-line
    "\C-e" 'evil-end-of-line
    "\C-r" 'undo-fu-only-redo)

  (evil-define-key nil evil-visual-state-map
    "\C-a" 'evil-beginning-of-line
    "\C-e" 'evil-end-of-line))

(use-package evil-nerd-commenter)

(use-package ivy
  :config (ivy-mode 1))

(use-package ivy-prescient
  :after (ivy prescient)
  :hook (ivy-mode . ivy-prescient-mode))

(use-package prescient)

(use-package projectile
  :after (ivy)
  :init (projectile-global-mode)
  :config
  (projectile-global-mode 1)
  (setq projectile-auto-discover t)
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  (setq projectile-project-search-path '("~/.emacs.d" "~/src" "~/personalsrc")))

(use-package undo-fu)

;;
;; language configuration
;;
(use-package lsp-mode
  :config
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-snippet nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-lens-enable nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-signature-auto-activate nil)
  (lsp-modeline-diagnostics-mode nil))
  
(use-package lsp-ui
  :after (lsp-mode)
  :config
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-position 'at-point)
  (setq lsp-ui-sideline-enable t))

;; python
(use-package lsp-python-ms
  :after (lsp-mode)
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp-deferred))))

;; rust
(use-package rust-mode
  :after (lsp-mode)
  :config
  (setq lsp-rust-server 'rust-analyzer)
  ;; (advice-add 'rust-ordinary-lt-gt-p
  ;; 	      :around
  ;; 	      (lambda (orig-func &rest args)
  ;; 		nil))
  (setq rust-match-angle-brackets nil)
  (add-hook 'rust-mode-hook #'lsp-deferred))

;;
;; keymap configuration
;;
;;
;; reregister keys
;; (crockeo-mode-register-keys crockeo-mode-map)
;;
(defvar crockeo-mode-keymap
  '(("C-SPC" lsp-ui-doc-glance)

    ("C-c c" evilnc-comment-or-uncomment-lines)

    ("C-c d" dired-sidebar-toggle-sidebar)

    ("C-c i" ibuffer)

    ("C-c p a" counsel-projectile-ag)
    ("C-c p f" projectile-find-file)
    ("C-c p p" projectile-switch-project)
    ("C-c p r" projectile-discover-projects-in-search-path)

    ("C-c j b" xref-pop-marker-stack)
    ("C-c j g" lsp-find-definition)

    ("C-c s" lsp-format-buffer)

    ("C-c w b" winpop-pop)
    ("C-c w c" winpop-clear)
    ("C-c w h" winpop-go-home)
    ("C-c w p" winpop-push)

    ("C-c 1 1" profiler-start)
    ("C-c 1 2" profiler-stop)
    ("C-c 1 3" profiler-report)))

(defun crockeo-mode-register-keys (keymap)
  (mapc (lambda (keybind)
	  (define-key keymap
	    (kbd (car keybind))
	    (cadr keybind)))
	crockeo-mode-keymap)
  keymap)

(define-minor-mode crockeo-mode
  "Defines my keymaps across all modes :)"
  :keymap (crockeo-mode-register-keys (make-sparse-keymap)))

(define-globalized-minor-mode global-crockeo-mode
  crockeo-mode
  (lambda ()
    (crockeo-mode 1)))

(global-crockeo-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("45b683f5a62a8fb5c6add97b7535687f3a1fd5bb1bdddd6a8a9510633271a4f8" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
