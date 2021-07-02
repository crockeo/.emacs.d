;;; go.el -*- lexical-binding: t; -*-

(ch/pkg-lang go ("\\.go\\'")
  (use-package go-mode
    :init (progn
	    (go-mode)
	    (lsp-deferred))
    :config
    ;; TODO: figure out how to NOT need this
    ;; in both :config and the hook below
    (setq tab-width 4))

  (defun ch/go/config ()
    (setq tab-width 4)
    (lsp-deferred))

  (add-hook 'go-mode-hook #'ch/go/config))
