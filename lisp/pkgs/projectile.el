(ch/pkg projectile
  ;; TODO: actually use this :)
  ;; it's a faster way of doing project discovery because it stops after the first .git
  ;; but i don't know how to put it into projectile :(
  (defun ch/projectile/project-dirs (&optional root-dir depth)
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
	      (message "%s" file)
	      (let ((qualified-name (concat root-dir "/" file)))
		(when (file-directory-p qualified-name)
		  (setf project-dirs
			(append project-dirs
				(ch/projectile/project-dirs qualified-name
							    (+ depth 1))))))))

	  project-dirs))))

  (defun ch/projectile/current-file ()
    (file-relative-name (buffer-file-name) (projectile-project-root)))

  (defun ch/projectile/copy-current-file ()
    (interactive)
    (let ((filename (ch/projectile/current-file)))
      (kill-new filename)
      (message "Copied to clipboard: '%s'" filename)))

  (use-package projectile
    :init
    (setq projectile-project-search-path
	  `((,(expand-file-name "~/src/") . 2)))
    (projectile-discover-projects-in-search-path))

  (use-package counsel-projectile
    :after (ivy projectile)
    :config
    (setq counsel-ag-base-command '("ag" "--hidden" "--vimgrep" "%s" "."))))
