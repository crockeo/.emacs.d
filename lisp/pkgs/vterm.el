;;; vterm.el -*- lexical-binding: t; -*-

(ch/pkg vterm
  (use-package vterm
    :config
    (when (require 'evil nil 'noerror)
      (evil-set-initial-state 'vterm-mode 'emacs))
    :hook
    (vterm-mode . (lambda () (display-line-numbers-mode -1)))))
