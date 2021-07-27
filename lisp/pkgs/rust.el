;;; rust.el -*- lexical-binding: t; -*-

(ch/pkg rust
  (use-package rust-mode
    :config
    (setq lsp-rust-analyzer-inlay-hints-mode t)
    :hook (rust-mode . (lambda ()
			 (setq lsp-rust-server 'rust-analyzer)
			 (lsp-deferred)))))
