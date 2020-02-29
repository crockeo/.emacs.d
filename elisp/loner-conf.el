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
(use-package srcery-theme)

;; macOS friendly alternative to pos-tip.
(use-package posframe)
