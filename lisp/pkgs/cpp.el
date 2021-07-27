;;; cpp.el -*- lexical-binding: t; -*-

(ch/pkg cpp
  (add-hook 'c++-mode-hook #'lsp-deferred))
