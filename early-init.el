;;; early-init.el -*- lexical-binding: t; -*-

(setq gc-cons-threshold (* 8 1024 1024 1024)) ;; 4GB

(setq frame-inhibit-implied-resize t
      menu-bar-mode nil
      package-enable-at-startup nil
      tool-bar-mode nil)

(set-scroll-bar-mode nil)
