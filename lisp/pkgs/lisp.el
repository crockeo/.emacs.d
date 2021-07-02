;;; lisp.el -*- lexical-binding: t; -*-


(ch/pkg lisp
  (use-package eros
    :hook ((emacs-lisp-mode lisp-mode) . eros-mode))

  (use-package paredit
    :hook ((emacs-lisp-mode lisp-mode) . paredit-mode))

  (add-hook 'lisp-mode-hook (lambda () (setq inferior-lisp-program "sbcl")))

  (use-package slime
    :hook (lisp-mode . slime)))
