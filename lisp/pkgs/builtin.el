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

  (run-with-idle-timer
   30 t
   (lambda ()
     (garbage-collect-maybe 3)))

  (global-auto-revert-mode 1)
  (global-display-line-numbers-mode 1)
  (global-eldoc-mode -1))
