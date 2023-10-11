;;; lsp.el -*- lexical-binding: t; -*-

(ch/pkg lsp
  (use-package lsp-mode)
  (use-package lsp-ui)
  (use-package lsp-ivy)
  (use-package lsp-treemacs)

  (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection "pyright-langserver")
                      :activation-fn (lsp-activate-on "python")
                      :server-id 'pyright)))
  )
