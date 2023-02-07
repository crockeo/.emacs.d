;;; rust.el -*- lexical-binding: t; -*-

(ch/pkg rust
  (use-package rust-mode
    :config
    :hook (rust-mode . eglot-ensure)))
