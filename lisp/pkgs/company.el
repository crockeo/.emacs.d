;; company.el -*- lexical-binding: t; -*-

(ch/pkg company (prog-mode-hook)
  (use-package company
    :hook (prog-mode . company-mode)
    :config
    (setq company-idle-delay 0))

  (use-package company-box
    :hook (company-mode . company-box-mode)))
