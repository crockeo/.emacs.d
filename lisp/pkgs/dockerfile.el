;;; dockerfile.el -*- lexical-binding: t; -*-

(ch/pkg dockerfile
  (use-package dockerfile-mode
    :config
    (setq dockerfile-enable-auto-indent nil)))
