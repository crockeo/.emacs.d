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

;;
;; builtin configuration
;;
(column-number-mode t)
(global-display-line-numbers-mode 1)
(global-auto-revert-mode t)
(tool-bar-mode -1)

(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq backup-directories-alist `((".*" . ,temporary-file-directory)))
(setq exec-path (append exec-path '("/usr/local/bin")))
(setq flymake-no-changes-timeout 5)
(setq initial-buffer-choice "~/home.org")
(setq read-process-output-max (* 2 1024 1024))
(setq ring-bell-function 'ignore)

(load-theme 'whiteboard)

;;
;; editor configuration
;;
(use-package company
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.05)
  (setq company-minimum-prefix-length 1)
  :hook (lsp-mode . (lambda () (push 'company-capf company-backends))))

(use-package company-prescient
  :after (company prescient))

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
    "\C-a" 'evil-beginning-of-line
    "\C-e" 'evil-end-of-line)

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

;;
;; language configuration
;;
(use-package lsp-mode
  :config
  (setq lsp-idle-delay
  :hook (rust-mode . #'lsp-mode-deferred))
  
(use-package lsp-ui
  :after (lsp-mode)
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-doc-position 'at-point))

;; python
(use-package lsp-python-ms
  :after (lsp-mode)
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
			 (require 'lsp-python-ms)
			 (lsp-deferred))))

;; rust
(use-package rust-mode
  :after (lsp-mode))

;;
;; keymap configuration
;;
;;
;; reregister keys
;; (crockeo-mode-register-keys crockeo-mode-map)
;;
(defvar crockeo-mode-keymap
  '(("C-c c" evilnc-comment-or-uncomment-lines)

    ("C-c i" ibuffer)

    ("C-c p a" projectile-ag)
    ("C-c p f" projectile-find-file)
    ("C-c p p" projectile-switch-project)
    ("C-c p r" projectile-discover-projects-in-search-path)

    ("C-c w b" (lambda ()
		 (interactive)
		 (ivy-set-view (ivy-pop-view))))
    ("C-c w h" (lambda ()
		 (interactive)
		 (ivy-push-view)
		 (delete-other-windows)
		 (find-file "~/home.org")))
    ("C-c w p" ivy-push-view)))

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
