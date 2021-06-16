;;; org.el -*- lexical-binding: t; -*-


(ch/pkg org (org-mode-hook)
  (defun ch/org/config ()
    (setq org-adapt-indentation nil))
  
  (use-package org
    :init (ch/org/config)
    :hook (org-mdoe . ch/org/conifg)))
