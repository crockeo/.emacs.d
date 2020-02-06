;; Installing packages
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Disable package format nits for elisp.
(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
