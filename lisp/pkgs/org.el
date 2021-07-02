;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defun ch/org/config ()
    (setq org-adapt-indentation nil
          org-hide-emphasis-markers t)

    (auto-fill-mode 0)
    (display-line-numbers-mode 0)
    (org-indent-mode 1)
    (visual-line-mode 1))

  (use-package org
    :hook (org-mode . ch/org/config)))
