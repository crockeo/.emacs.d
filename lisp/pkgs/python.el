;;; python.el -*- lexical-binding: t; -*-

(ch/pkg python
  (defun ch/python/lyftvenv ()
    (interactive)

    (let ((default-directory (projectile-project-root)))
      (when default-directory
	(with-temp-buffer
	  (start-process "lyftvenv" "*lyftvenv*"
			 "lyftvenv")))))

  (use-package lsp-pyright
    :init (setq lsp-pyright-multi-root nil)
    :hook (python-mode . (lambda ()
			   (require 'lsp-pyright)
			   (let ((venv-dir (concat (projectile-project-root) "venv")))
			     (when (file-directory-p venv-dir)
			       (pyvenv-activate venv-dir)))
			   (lsp-deferred)))
    :config
    (setq lsp-pyright-log-level "error"
	  lsp-pyright-multi-root nil))

  (use-package pyvenv))
