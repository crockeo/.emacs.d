;;; olivetti.el -*- lexical-binding: t; -*-

(ch/pkg olivetti
  (use-package olivetti
    :config
    (setq olivetti-minimum-body-width 120))

  (defvar ch/olivetti/enabled nil)

  (defun ch/olivetti/ensure ()
    (olivetti-mode
     (if ch/olivetti/enabled
	 nil
       -1)))

  (defun ch/olivetti/toggle-ensure ()
    (interactive)
    (if ch/olivetti/enabled
	(setq ch/olivetti/enabled nil)
      (setq ch/olivetti/enabled t))
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
	(ch/olivetti/ensure))))

  (add-hook 'buffer-list-update-hook #'ch/olivetti/ensure))
