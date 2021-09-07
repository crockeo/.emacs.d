;;; olivetti.el -*-; lexical-binding: t; -*-

(ch/pkg olivetti
  (defun ch/olivetti/all-horizontal ()
    (car (and (mapcar #'window-full-width-p (window-list)))))

  (defun ch/olivetti/toggle-visible-windows (mode-toggle)
    (mapc
     (lambda (window)
       (with-current-buffer (window-buffer window)
	 (olivetti-mode mode-toggle)))
     (window-list)))

  (defun ch/olivetti/auto-olivetti (window)
    (let ((mode-toggle (if (ch/olivetti/all-horizontal)
			   1
			 -1)))
      (ch/olivetti/toggle-visible-windows mode-toggle)))

  (use-package olivetti
    :config
    (setq olivetti-minimum-body-width 240))

  ;; (remove-hook 'window-size-change-functions #'ch/olivetti/auto-olivetti)
  (add-hook 'window-size-change-functions #'ch/olivetti/auto-olivetti))
