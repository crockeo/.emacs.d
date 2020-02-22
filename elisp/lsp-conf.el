(defun conf-lsp-mode ()
  (use-package lsp-mode)

  ;; Setting up package modes
  (use-package company-lsp
    :after company lsp-mode
    :config '(push 'company-lsp company-backends))

  (use-package helm-lsp)

  (use-package lsp-ui
    :after lsp-mode)

  ;; Setting up language modes
  (add-hook 'go-mode-hook 'lsp)
  (add-hook 'python-mode-hook 'lsp))

(unless (version< emacs-version "27")
  (conf-lsp-mode))
