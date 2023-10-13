;;; javascript.el -*- lexical-binding: t; -*-

(ch/pkg javascript
  ;; TODO: this stuff is now managed by treesitter.erl
  ;; (use-package typescript-mode)
  ;; (use-package web-mode
  ;;   :mode (("\\.tsx?\\'" . web-mode))
  ;;   :config
  ;;   (setq web-mode-code-indent-offset 2))

  ;; (defun ch/javascript/configure-indentation ()
  ;;   (setq indent-tabs-mode nil)
  ;;   (setq tab-width 2)
  ;;   (setq js-indent-level 2)
  ;;   (setq typescript-indent-level 2))

  ;; (defun ch/javascript/on-load ()
  ;;   (ch/javascript/configure-indentation)
  ;;   (eglot-ensure))

  ;; (add-hook 'js-mode-hook #'ch/javascript/on-load)
  ;; (add-hook 'typescript-mode-hook #'ch/javascript/on-load)
  )
