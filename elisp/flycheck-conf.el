;; Installing packages
(use-package flycheck
  :init (global-flycheck-mode))

;; Disable package format nits for elisp.
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;; Increase the number of errors the flycheck can render.
(setq flycheck-checker-error-threshold 2048)
