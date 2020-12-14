(use-package rust-mode
  :hook (rust-mode . cargo-minor-mode)
  :config
  (setq lsp-rust-clippy-preference "on"))
(use-package cargo)
