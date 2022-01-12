;;; olivetti.el -*- lexical-binding: t; -*-

(ch/pkg olivetti
  (use-package olivetti
    :config
    (setq olivetti-minimum-body-width 120))

  (defvar ch/olivetti/enabled nil)

  (defun ch/olivetti/ensure ()
    (when (not (equal ch/olivetti/enabled olivetti-mode))
     (olivetti-mode
      (if ch/olivetti/enabled t -1))))

  (defun ch/olivetti/toggle-ensure ()
    (interactive)
    (setq ch/olivetti/enabled (not ch/olivetti/enabled))
    (dolist (window (window-list))
      (with-current-buffer (window-buffer window)
	(ch/olivetti/ensure))))

  (add-hook 'buffer-list-update-hook #'ch/olivetti/ensure))
