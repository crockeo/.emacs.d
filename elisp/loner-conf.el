;; Provides jump-to-definition
(use-package dumb-jump
  :init (dumb-jump-mode))

;; Loading editorconfig from repo
(use-package editorconfig
  :config (editorconfig-mode 1))

;; Evaluation at end of line
(use-package eros
  :init (eros-mode))

;; Makes emacs pretty
(load-library "hawaii-theme")
(load-theme 'hawaii t)

;; macOS friendly alternative to pos-tip.
(use-package posframe)

;; Inherit environment variables from the shell.
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Syntax for Dockerfiles
(use-package dockerfile-mode)
