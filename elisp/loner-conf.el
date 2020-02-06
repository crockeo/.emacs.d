;; Provides jump-to-definition
(use-package dumb-jump
  :ensure t
  :init (dumb-jump-mode))

;; Loading package
(use-package eros
  :ensure t
  :init (eros-mode))

;; Makes emacs pretty
(use-package monokai-theme
  :ensure t)

;; macOS friendly alternative to pos-tip.
(use-package posframe
  :ensure t)
