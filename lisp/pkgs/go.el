;;; go.el -*- lexical-binding: t; -*-

(ch/pkg go
  (use-package go-mode)

  ;; putting this as :hook inside of use-package
  ;; causes issues, so i'm not doing it! :)
  (defun ch/go/config ()
    (setq tab-width 4)
    (lsp-deferred))

  (add-hook 'go-mode-hook #'ch/go/config))
