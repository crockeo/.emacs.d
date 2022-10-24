;;; vterm.el -*- lexical-binding: t; -*-

(ch/pkg vterm
  (use-package vterm
    :config
    (evil-set-initial-state 'vterm-mode 'emacs))

  (defun ch/vterm/config ()
    (diff-hl-mode -1)
    (display-line-numbers-mode -1))

  (add-hook 'vterm-mode-hook #'ch/vterm/config))
