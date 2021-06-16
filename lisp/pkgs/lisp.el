;;; lisp.el -*- lexical-binding: t; -*-


(ch/pkg lisp (emacs-lisp-mode-hook)
  (use-package eros
    :init (eros-mode 1)
    :hook (emacs-lisp-mode . eros-mode))

  (use-package paredit
    :init (paredit-mode 1)
    :hook (emacs-lisp-mode . paredit-mode)))
