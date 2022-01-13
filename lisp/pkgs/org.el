;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (setq org-directory (expand-file-name "~/org"))

  (defun ch/org/ensure-org-directory ()
    (make-directory org-directory t))

  (defun ch/org/push-files ()
    (interactive)
    (ch/org/ensure-org-directory)
    (make-thread
     (lambda ()
       (ch/under-dir org-directory
	 (magit-git "add" ".")
	 (magit-git "commit" "-m" (format "%s -- syncing org files" (current-time-string)))
	 (magit-git "push" "-u" "origin" (magit-get-current-branch))))))

  (defun ch/org/pull-files ()
    (interactive)
    (ch/org/ensure-org-directory)
    (make-thread
     (lambda ()
       (ch/under-dir org-directory
	 (magit-git "pull" "origin" (magit-get-current-branch)))))))
