;;; javascript.el -*- lexical-binding: t; -*-

(ch/pkg javascript
  (use-package typescript-mode)
  (use-package web-mode
    :mode (("\\.tsx?\\'" . web-mode))
    :config
    (setq web-mode-code-indent-offset 2))

  (defun ch/javascript/configure-indentation ()
    (setq indent-tabs-mode nil)
    (setq tab-width 2)
    (setq js-indent-level 2)
    (setq typescript-indent-level 2))

  (defun ch/javascript/on-load ()
    (ch/javascript/configure-indentation)
    (lsp-deferred))

  ;; (add-to-list 'auto-mode-alist '(".tsx\\'" . typescript-mode))

  (add-hook 'js-mode-hook #'ch/javascript/on-load)
  (add-hook 'typescript-mode-hook #'ch/javascript/on-load))
