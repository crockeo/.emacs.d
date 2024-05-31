;; company.el -*- lexical-binding: t; -*-

(ch/pkg company
  (use-package company
    :hook (prog-mode . company-mode)
    :config
    (setq company-minimum-prefix-length 1)
    (setq company-idle-delay 0))

  (use-package company-box
    :after (company)
    :hook (company-mode . company-box-mode)))
