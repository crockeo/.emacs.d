;; Installing pacakges
(use-package yasnippet)

(use-package lsp-mode
  :after yasnippet
  :config
  (setq lsp-eldoc-hook nil)
  (setq lsp-prefer-capf t)
  (setq lsp-signature-auto-activate nil)

  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'python-mode-hook 'lsp-deferred))

(use-package helm-lsp
  :after helm lsp-mode)

(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode-hook . lsp-ui-mode))
