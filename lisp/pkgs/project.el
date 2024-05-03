;;; project.el -*- lexical-binding: t; -*-

(ch/pkg project
  (use-package project
    :bind
    (
     ("C-c p c" . ch/project/copy-current-file)
     ("C-c p f" . project-find-file)
     ("C-c p m" . ch/project/copy-current-module)
     ("C-c p p" . project-switch-project)
     ("C-c p r" . ch/project/discover-projects)
     ("C-c p s" . project-query-replace-regexp)
     )

    :config
    (defun ch/project/current-file ()
      (file-relative-name (buffer-file-name) (project-root (project-current))))

    (defun ch/project/copy-current-file ()
      (interactive)
      (let ((filename (ch/project/current-file)))
        (kill-new filename)
        (message "Copied to clipboard: '%s'" filename)))

    (defun ch/project/current-module ()
      "Gets the current module name, assuming a Python-style module syntax"
      (--> (ch/project/current-file)
           (s-chop-suffix ".py" it)
           (s-chop-suffix "/__init__" it)
           (s-replace "/" "." it)))

    (defun ch/project/copy-current-module ()
      (interactive)
      (let ((module-name (ch/project/current-module)))
        (kill-new module-name)
        (message "Copied to clipboard: '%s'" module-name)))

    (defun ch/project/project-dirs (&optional root-dir depth)
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
				  (ch/project/project-dirs qualified-name
							   (+ depth 1))))))))

	    project-dirs))))

    (defun ch/project/discover-projects ()
      "Discovers all of the projects under the known project roots and adds them to project.el."
      (interactive)
      (dolist (project (project-known-project-roots))
        (project-forget-project project))
      (dolist (project (ch/project/project-dirs))
        (project-remember-projects-under project)))

    (ch/project/discover-projects)
    (setq project-switch-commands #'project-find-file)))
