;;; early-init.el -*- lexical-binding: t; -*-

(setq ch/original-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 1024 2)) ;; 2GB

(setq frame-inhibit-implied-resize t
      menu-bar-mode nil
      package-enable-at-startup nil
      tool-bar-mode nil)

(set-scroll-bar-mode nil)
