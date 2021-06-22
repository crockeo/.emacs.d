;;; go.el -*- lexical-binding: t; -*-

;; TODO: figure out and generalize this nonsense

(define-derived-mode go-bootstrap-mode fundamental-mode
  "Major mode used for bootstrapping golang packaging.")

(push '("\\.go\\'" . go-bootstrap-mode) auto-mode-alist)

(ch/pkg go (go-bootstrap-mode-hook)
  (use-package go-mode
    :init (progn
	    (go-mode)
	    (lsp-deferred)))

  (add-hook 'go-mode-hook #'lsp-deferred))
