;; lsp.el -*- lexical-binding: t; -*-

(ch/pkg lsp (python-mode-hook)
  (use-package lsp-mode
    :init (setq lsp-keymap-prefix "C-c l"))

  ;; TODO: figure out how i want to
  ;; organize language-specific packages for LSP
  (use-package lsp-python-ms
    :hook (python-mode . lsp-deferred)
    :config (setq lsp-python-ms-auto-install-server t)))
