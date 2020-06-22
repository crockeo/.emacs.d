;; Installing pacakges
(use-package lsp-mode
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

(use-package lsp-python-ms
  :after lsp-mode)
