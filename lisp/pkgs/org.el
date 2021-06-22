;;; org.el -*- lexical-binding: t; -*-


(ch/pkg org (org-mode-hook)
  (defun ch/org/modes ()
    (auto-fill-mode 0)
    (display-line-numbers-mode 0)
    (org-indent-mode 1)
    (visual-line-mode 1))

  (defun ch/org/config ()
    (setq org-adapt-indentation nil
          org-hide-emphasis-markers t))

  (use-package org
    :init (ch/org/modes)
    :config (ch/org/config)
    :hook (org-mode . ch/org/modes)))
