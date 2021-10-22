;;; olivetti.el -*-; lexical-binding: t; -*-

(ch/pkg olivetti
  (defun ch/olivetti/auto-olivetti (window)
    (if (= (length (window-list)) 1)
	(olivetti-mode 1)
      (olivetti-mode -1)))

  (use-package olivetti
    :config
    (setq olivetti-minimuma-body-width 120))

  ;; NOTE: this is actually kind of breaking things
  ;; unfortunately :(
  ;; (add-hook 'window-size-change-functions #'ch/olivetti/auto-olivetti)
  )
