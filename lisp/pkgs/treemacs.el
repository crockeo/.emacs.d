;;; treemacs.el -*- lexical-binding: t; -*-

(ch/pkg treemacs
  (use-package treemacs
    :bind (("C-c d" . treemacs))
    :hook (treemacs-mode . (lambda () (display-line-numbers-mode -1)))))
