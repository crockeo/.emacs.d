;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 1024 1024 1024 8)) ;; 8GB

(setq frame-inhibit-implied-resize t
      menu-bar-mode nil
      package-enable-at-startup nil
      tool-bar-mode nil)

(set-scroll-bar-mode nil)
