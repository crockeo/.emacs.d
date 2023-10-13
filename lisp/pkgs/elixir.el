;;; elixir.el -*- lexical-binding: t; -*-

(ch/pkg elixir
  (use-package elixir-mode
    :init
    (add-hook 'elixir-mode-hook #'lsp-deferred)
    (add-to-list 'exec-path (expand-file-name "~/bin/elixir-ls-folder/")))

  ;; (use-package elixir-ts-mode
  ;;   :init
  ;;   (add-hook 'elixir-ts-mode-hook #'lsp-deferred)
  ;;   (add-to-list 'exec-path (expand-file-name "~/bin/elixir-ls-folder/")))
  )
