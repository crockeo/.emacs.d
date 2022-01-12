;;; zenburn.el -*- lexical-binding: t; -*-

(ch/pkg zenburn
  (use-package zenburn-theme
    :config
    (setq zenburn-scale-org-headlines t
	  zenburn-use-variable-pitch t)
    (load-theme 'zenburn t))

  (defun ch/zenburn/color (name)
    (cdr (assoc (concat "zenburn-" name) zenburn-default-colors-alist))))
