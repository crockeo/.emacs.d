;;; diff-hl.el -*- lexical-binding: t; -*-

(ch/pkg diff-hl
  (use-package diff-hl
    :init (global-diff-hl-mode)
    :hook (diff-hl-mode . diff-hl-flydiff-mode)))
