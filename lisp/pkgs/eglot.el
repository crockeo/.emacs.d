;;; eglot.el -*- lexical-binding: t; -*-

(ch/pkg eglot
  (use-package eglot
    :config
    (setq eglot-events-buffer-size 0)
    )

  (defun ch/eglot/config ()
    (setq eldoc-documentation-functions
	  (cons #'flymake-eldoc-function
		(remove #'flymake-eldoc-function eldoc-documentation-functions)))

    (setq eldoc-documentation-strategy #'eldoc-documentation-compose)

    (setq eglot-events-buffer-size 0))

  (defvar ch/eglot/debug nil)

  (defun ch/eglot/disable-logging (orig-fun &rest args)
    (when ch/eglot/debug
      (apply orig-fun args)))

  (advice-add 'ch/eglot/disable-logging :around #'jsonrpc--log-event)

  (add-hook 'eglot-managed-mode-hook #'ch/eglot/config)

  )
