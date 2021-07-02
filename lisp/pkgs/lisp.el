;;; lisp.el -*- lexical-binding: t; -*-


(ch/pkg lisp
  (use-package eros
    :hook ((emacs-lisp-mode lisp-mode) . eros-mode))

  (use-package paredit
    :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))

  (use-package slime))
