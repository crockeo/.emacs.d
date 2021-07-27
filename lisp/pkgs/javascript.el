;;; javascript.el -*- lexical-binding: t; -*-

(ch/pkg javascript
  (use-package typescript-mode)

  (add-hook 'javascript-mode-hook #'lsp-deferred)
  (add-hook 'typescript-mode-hook #'lsp-deferred))
