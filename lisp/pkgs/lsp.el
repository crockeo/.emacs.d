;; lsp.el -*- lexical-binding: t; -*-

(ch/pkg lsp (python-mode-hook)
  (use-package lsp-mode
    :init (setq lsp-keymap-prefix "C-c l")
    :config
    (setq lsp-enable-on-type-formatting nil
	  lsp-enable-symbol-highlighting nil
	  lsp-headerline-breadcrumb-enable nil
	  lsp-idle-delay 1.0))

  (use-package lsp-ui)

  ;; TODO: figure out how i want to
  ;; organize language-specific packages for LSP
  (use-package lsp-python-ms
    :config (setq lsp-python-ms-auto-install-server t)
    :hook (python-mode . (lambda ()
			   (require 'lsp-python-ms)
			   (let ((venv-dir (concat (projectile-project-root) "venv")))
			     (when (file-directory-p venv-dir)
			       (pyvenv-activate venv-dir)))
			   (lsp))))

  (use-package pyvenv))
