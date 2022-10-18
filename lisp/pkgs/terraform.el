;;; terraform.el -*- lexical-binding: t; -*-

(ch/pkg terraform
  (use-package terraform-mode
    :after lsp)

  ;; (add-hook 'terraform-mode-hook #'lsp-deferred)
  (remove-hook 'terraform-mode-hook #'lsp-deferred)
  )
