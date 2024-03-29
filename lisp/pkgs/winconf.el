;;; winconf.el -*- lexical-binding: t; -*-

(ch/pkg winconf
  (defvar ch/winconf/state nil)

  (defun ch/winconf/save (&optional winconf)
    (interactive)
    (if ch/winconf/state
	(message "Can't overwrite existing window configuration.")
      (progn
       (setq ch/winconf/state
	     (or winconf (current-window-configuration)))
       (message "Saved current window configuration."))))

  (defun ch/winconf/pop (&optional winconf)
    (interactive)
    (let ((target (or winconf ch/winconf/state)))
     (if target
	 (progn
	   (set-window-configuration target)
	   (setq ch/winconf/state nil)
	   (message "Restored prior window configuration."))
       (message "No prior window configuration.")))))
