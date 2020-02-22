;; Installing pacakges
(use-package lsp-mode)

(use-package company-lsp
  :after company
  :config '(push 'company-lsp company-backends))

(use-package helm-lsp)

(use-package lsp-ui)

;; Setting up languages
(add-hook 'go-mode-hook 'lsp-deferred)
(add-hook 'python-mode-hook 'lsp-deferred)

;; Misc configuration
(setq lsp-eldoc-hook nil)
(setq lsp-signature-auto-activate nil)
