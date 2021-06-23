;;; python.el -*- lexical-binding: t; -*-

(ch/pkg python (python-mode-hook)
  (defun ch/python/lyftvenv ()
    (interactive)

    (let ((default-directory (projectile-project-root)))
      (when default-directory
	(with-temp-buffer
	  (start-process "lyftvenv" "*lyftvenv*"
			 "lyftvenv")))))

  (use-package lsp-pyright
    :hook (python-mode . (lambda ()
			   (require 'lsp-pyright)
			   (let ((venv-dir (concat (projectile-project-root) "venv")))
			     (when (file-directory-p venv-dir)
			       (pyvenv-activate venv-dir)))
			   (lsp))))

  (use-package pyvenv))
