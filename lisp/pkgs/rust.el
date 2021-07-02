;;; rust.el -*- lexical-binding: t; -*-

(ch/pkg rust
  (use-package rust-mode
    :hook (rust-mode . (lambda ()
			 (setq lsp-rust-server 'rust-analyzer)
			 (lsp-deferred)))))
