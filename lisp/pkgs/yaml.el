;;; yaml.el -*- lexical-binding: t; -*-

(ch/pkg yaml
  (use-package yaml-mode
    :config
    (add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode))))
