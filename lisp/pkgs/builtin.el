(ch/pkg builtin ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  (set-face-attribute 'default nil
		      :font "Fira Mono"
		      :height 140)

  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
	exec-path (split-string (getenv "PATH") ":")
	gc-cons-threshold (* 1024 1024 1024 2) ;; 2GB
	inhibit-startup-screen t
	read-process-output-max (* 1024 1024 4) ;; 2MB
	ring-bell-function 'ignore)

  (global-display-line-numbers-mode 1))
