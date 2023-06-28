;;; clojure.el -*- lexical-binding: t; -*-

(ch/pkg clojure
  (use-package clojure-mode
    :config
    (add-hook 'clojure-mode-hook #'eglot-ensure)
    (add-hook 'clojurescript-mode-hook #'eglot-ensure)
    (add-hook 'clojurec-mode-hook #'eglot-ensure)

    (add-hook 'clojure-mode-hook #'paredit-mode)

    )
  (use-package cider
    :after clojure-mode)
  )
