;;; theme.el -*- lexical-binding: t; -*-

(ch/pkg theme
  (use-package zenburn-theme
    :config
    (setq zenburn-scale-org-headlines t
	  zenburn-use-variable-pitch t))

  (defvar ch/theme/day-theme 'modus-operandi)
  (defvar ch/theme/night-theme 'zenburn)

  (setq modus-themes-hl-line 'accented)

  (defun ch/theme/current-local-time ()
    (let ((offset (car (current-time-zone)))
	  (now (time-convert (current-time) 'integer)))
      (+ offset now)))

  (defun ch/theme/hour-min-sec ()
   (let ((now (ch/theme/current-local-time)))
     (list (% (/ now 60 60) 24)
	   (% (/ now 60) 60)
	   (% now (* 60)))))

  (defun ch/theme/use-darkmode ()
    ;; TODO: find out how to get time of sunset in current timezone / location
    (pcase-let
	((`(,hour ,_ ,_) (ch/theme/hour-min-sec)))
      (or (< hour 6)
	  (>= hour 18))))

  (defun ch/theme/set-theme ()
    (if (ch/theme/use-darkmode)
	(progn
	  (disable-theme ch/theme/day-theme)
	  (load-theme ch/theme/night-theme t))
      (progn
	(disable-theme ch/theme/night-theme)
	(load-theme ch/theme/day-theme t))))

  (run-with-timer 0 (* 60 60) #'ch/theme/set-theme))
