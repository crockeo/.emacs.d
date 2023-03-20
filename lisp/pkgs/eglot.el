;;; eglot.el -*- lexical-binding: t; -*-

(ch/pkg eglot
  (use-package eglot)

  (defun ch/eglot/config ()
    (setq eldoc-documentation-functions
	  (cons #'flymake-eldoc-function
		(remove #'flymake-eldoc-function eldoc-documentation-functions))

	  eldoc-documentation-strategy
	  #'eldoc-documentation-compose))

  (add-hook 'eglot-managed-mode-hook #'ch/eglot/config)

  )
