;;; early-init.el -*- lexical-binding: t; -*-

;; lsp-mode documentation suggests that you set LSP_USE_PLISTS as early as possible.
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
(setenv "LSP_USE_PLISTS" "true")

(setq gc-cons-threshold (* 8 1024 1024 1024)) ;; 4GB

(setq frame-inhibit-implied-resize t
      menu-bar-mode nil
      package-enable-at-startup nil
      tool-bar-mode nil)

(set-scroll-bar-mode nil)
