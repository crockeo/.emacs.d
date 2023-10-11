;;; elixir.el -*- lexical-binding: t; -*-

(ch/pkg elixir
  (use-package elixir-ts-mode
    :init
    (add-hook 'elixir-ts-mode-hook #'lsp-deferred)))
