;;; theme.el -*- lexical-binding: t; -*-

(ch/pkg theme
  (use-package zenburn-theme
    :config
    (setq zenburn-scale-org-headlines t
	  zenburn-use-variable-pitch t)
    (load-theme 'zenburn t))

  (defun ch/theme/zenburn-color (name)
    (cdr (assoc (concat "zenburn-" name) zenburn-default-colors-alist)))
  )
