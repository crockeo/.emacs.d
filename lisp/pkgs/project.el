;;; project.el -*- lexical-binding: t; -*-

(ch/pkg project
  (require 'project)
  (with-eval-after-load 'project
    (defun ch/builtin/current-file ()
      (file-relative-name (buffer-file-name) (project-root (project-current))))

    (defun ch/builtin/copy-current-file ()
      (interactive)
      (let ((filename (ch/builtin/current-file)))
        (kill-new filename)
        (message "Copied to clipboard: '%s'" filename)))

    (defun ch/builtin/project-dirs (&optional root-dir depth)
      "Discovers all of the projects under ROOT-DIR."
      (cl-block project-dirs
        (let* ((depth (or depth 0))
	       (root-dir (or root-dir (expand-file-name "~/src"))))
	  (when (> depth 3)
	    (cl-return-from project-dirs nil))

	  (let ((project-dirs nil))
	    (cl-dolist (file (directory-files root-dir))
	      (when (string= file ".git")
	        (cl-return-from project-dirs (list root-dir)))

	      (unless (or (string= file ".") (string= file ".."))
	        (let ((qualified-name (concat root-dir "/" file)))
		  (when (file-directory-p qualified-name)
		    (setf project-dirs
			  (append project-dirs
				  (ch/builtin/project-dirs qualified-name
							   (+ depth 1))))))))

	    project-dirs))))

    (defun ch/builtin/discover-projects ()
      "Discovers all of the projects under the known project roots and adds them to project.el."
      (dolist (project (project-known-project-roots))
        (project-forget-project project))
      (dolist (project (ch/builtin/project-dirs))
        (project-remember-projects-under project)))

    (ch/builtin/discover-projects)
    (setq project-switch-commands #'project-find-file)))
