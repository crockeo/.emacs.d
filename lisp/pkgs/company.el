;; company.el -*- lexical-binding: t; -*-

(ch/pkg company
  (use-package company
    :hook (prog-mode . company-mode)
    :config
    (setq company-idle-delay 0.25))

  (use-package company-box
    :after (company)
    :hook (company-mode . company-box-mode)))
