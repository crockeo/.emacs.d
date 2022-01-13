;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (setq org-directory (expand-file-name "~/org"))

  (defun ch/org/ensure-org-directory ()
    (make-directory org-directory t))

  ;; these functions help me synchronize my files across devices
  ;; by just sending them to and retrieving them from a git repo!
  (defun ch/org/download-files ()
    (interactive)
    (message "org.el: Downloading files...")
    (ch/org/ensure-org-directory)
    (make-thread
     (lambda ()
       (condition-case ()
	   (progn
	     (ch/under-dir org-directory
	       (call-process "git" nil nil nil "pull" "origin" (magit-get-current-branch)))
	     (message "org.el: Downloaded files!"))
	 (error (message "org.el: Failed to download files."))))))

  (defun ch/org/upload-files ()
    (interactive)
    (message "org.el: Uploading files...")
    (ch/org/ensure-org-directory)
    (make-thread
     (lambda ()
       (condition-case ()
	   (progn
	     (ch/under-dir org-directory
	       (call-process "git" nil nil nil "add" ".")
	       (call-process "git commit" "-m" (format "%s -- syncing org files" (current-time-string)))
	       (call-process "git push" "-u" "origin" (magit-get-current-branch)))
	     (message "org.el: Uploaded files!"))
	 (error (message "org.el: Failed to upload files.")))))))
