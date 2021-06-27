;;; go.el -*- lexical-binding: t; -*-

(ch/pkg-lang go ("\\.go\\'")
  (use-package go-mode
    :init (progn
	    (go-mode)
	    (lsp-deferred))
    :config
    (setq tab-width 4))

  (add-hook 'go-mode-hook #'lsp-deferred))
