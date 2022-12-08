;;; zig.el -*- lexical-binding: t; -*-

(ch/pkg zig
  (use-package zig-mode
    :config
    (setq zig-format-on-save nil))

  (add-hook 'zig-mode-hook #'eglot-ensure))
