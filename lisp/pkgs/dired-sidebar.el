;;; dired-sidebar.el -*- lexical-binding: t; -*-

(ch/pkg dired-sidebar
  (use-package dired-sidebar
    :bind (("C-c d" . dired-sidebar-toggle-sidebar))
    :hook (dired-mode . (lambda () (display-line-numbers-mode -1)))))
