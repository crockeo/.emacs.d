;; company.el -*- lexical-binding: t; -*-

(ch/pkg company (prog-mode-hook)
  (use-package company
    :hook (prog-mode . company-mode))
  
  (use-package company-box
    :hook (company-mode . company-box-mode)))
