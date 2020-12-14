;; (require 'ocaml-lsp-mode)

;; Installing pacakges
(use-package yasnippet)

(use-package lsp-mode
  :after yasnippet
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-prefer-capf nil)
  (setq lsp-signature-auto-activate nil)

  (mapc
   (lambda (mode)
     (add-hook mode 'lsp-deferred))
   '(c++-mode-hook
     caml-mode-hook
     go-mode-hook
     js-mode-hook
     python-mode-hook
     rust-mode-hook
     typescript-mode-hook))
  )

(use-package company-lsp
  :after lsp-mode)

(use-package helm-lsp
  :after helm lsp-mode)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode-hook . lsp-ui-mode))
