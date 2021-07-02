;;; yaml.el -*- lexical-binding: t; -*-

(ch/pkg-lang yaml ("\\.yaml\\'" "\\.yml\\'")
  (use-package yaml-mode
    :init (yaml-mode)))
