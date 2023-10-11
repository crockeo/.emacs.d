;;; clojure.el -*- lexical-binding: t; -*-

(ch/pkg clojure
  (use-package clojure-mode
    :config
    (add-hook 'clojure-mode-hook #'lsp-deferred)
    (add-hook 'clojurescript-mode-hook #'lsp-deferred)
    (add-hook 'clojurec-mode-hook #'lsp-deferred)

    (add-hook 'clojure-mode-hook #'paredit-mode)

    )
  (use-package cider
    :after clojure-mode)
  )
