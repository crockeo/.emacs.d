;; lsp.el -*- lexical-binding: t; -*-

(ch/pkg lsp (prog-mode-hook)
  (use-package lsp-mode
    :init (setq lsp-keymap-prefix "C-c l")
    :config
    (setq lsp-enable-on-type-formatting nil
	  lsp-enable-symbol-highlighting nil
	  lsp-headerline-breadcrumb-enable nil
	  lsp-idle-delay 0.25))

  (use-package lsp-ui))
