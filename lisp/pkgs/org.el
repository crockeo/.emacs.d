;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (setq org-directory (expand-file-name "~/org"))

  (defun ch/org/ensure-org-directory ()
    (make-directory org-directory t))

  (defun ch/org/push-files ()
    (interactive)
    (message "org.el: Pushing files...")
    (ch/org/ensure-org-directory)
    (make-thread
     (lambda ()
       (condition-case ()
	   (progn
	     (ch/under-dir org-directory
	       (call-process "git" nil nil nil "add" ".")
	       (call-process "git commit" "-m" (format "%s -- syncing org files" (current-time-string)))
	       (call-process "git push" "-u" "origin" (magit-get-current-branch)))
	     (message "org.el: Pushed files!"))
	 (error (message "org.el: Failed to push files."))))))

  (defun ch/org/pull-files ()
    (interactive)
    (message "org.el: Pulling files...")
    (ch/org/ensure-org-directory)
    (make-thread
     (lambda ()
       (condition-case ()
	   (progn
	     (ch/under-dir org-directory
	       (call-process "git" nil nil nil "pull" "origin" (magit-get-current-branch)))
	     (message "org.el: Pulled files!"))
	 (error (message "org.el: Failed to push files.")))))))
