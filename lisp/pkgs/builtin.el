;;; load-pkgs.el -*- lexical-binding: t; -*-

(ch/pkg builtin
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  ;; used when starting vterm
  ;; so we don't connect to tmux.
  ;; see .zshrc for info
  (setenv "EMACS" "true")

  ;; In general: use spaces.
  ;; Can use tabs for specific modes.
  (setq-default indent-tabs-mode nil)

  (defun ch/builtin/set-font (font-size)
    (condition-case nil
	(set-face-attribute 'default nil
			    :font "Menlo"
			    :height font-size)
      (error
       (set-face-attribute 'default nil
			   :height font-size))))

  (defvar ch/builtin/presentation-font-size 200)
  (defvar ch/builtin/normal-font-size 120)

  (defvar ch/builtin/is-presenting nil)

  (defun ch/builtin/toggle-present ()
    (interactive)
    (if ch/builtin/is-presenting
	(progn
	  (setq ch/builtin/is-presenting nil)
	  (ch/builtin/set-font ch/builtin/normal-font-size))
      (progn
	(setq ch/builtin/is-presenting t)
	(ch/builtin/set-font ch/builtin/presentation-font-size))))

  (ch/builtin/set-font ch/builtin/normal-font-size)

  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
	exec-path (split-string (getenv "PATH") ":")
	inhibit-startup-screen t
	read-process-output-max (* 1024 1024 4) ;; 4MB
	ring-bell-function 'ignore)

  (defun get-shell-path-entries ()
    (split-string
     (string-trim
      (with-temp-buffer
	(call-process "zsh"
		      nil
		      t
		      nil
		      "-c"
		      "export EMACS=1 && . ~/.zprofile && . ~/.zshrc && echo $PATH")
	(buffer-string)))
     ":"))

  (let ((shell-path-entries (get-shell-path-entries)))
    (setenv "PATH" (string-join shell-path-entries ":"))
    (dolist (path shell-path-entries)
      (push path exec-path)))

  (setq auto-save-file-name-transforms
	'((".*" "~/.emacs.d/autosave/" t)))

  (setq create-lockfiles nil)

  ;; TODO: do something similar for linux
  (when (string= system-type "darwin")
    (dolist (path '("~/.emacs.d/emacs/nextstep/Emacs.app/Contents/MacOS/libexec"
		    "~/.emacs.d/emacs/nextstep/Emacs.app/Contents/MacOS/bin"))
      (push (expand-file-name path)
	    exec-path)))

  (setq
   scroll-conservatively integer-width
   scroll-margin 4
   scroll-step 1
   )

  (defun ch/builtin/garbage-collect ()
    ;; NOTE: can make this `when t` to debug background garbage collection.
    ;; But it seems to be working pretty well :)
    (when nil
      (message "Background garbage collection: %s" (current-time-string)))

    (let ((original-gc-cons-threshold gc-cons-threshold))
      (setq gc-cons-threshold (* 200 1024 1024)) ;; 200MB
      (garbage-collect)
      (setq gc-cons-threshold original-gc-cons-threshold)))

  (defvar ch/builtin/garbage-collect-periodicity-seconds 60)

  (defun ch/builtin/garbage-collect-worker ()
    (let ((idle-time (float-time (or (current-idle-time) 0))))
      (when (> idle-time ch/builtin/garbage-collect-periodicity-seconds)
        (ch/builtin/garbage-collect))))

  (add-hook 'focus-out-hook #'ch/builtin/garbage-collect)
  (run-with-timer 0 60 #'ch/builtin/garbage-collect-worker)

  (global-auto-revert-mode 1)
  (global-display-line-numbers-mode 1)
  (global-eldoc-mode -1)
  (setq eldoc-documentation-function #'ignore)

  ;;
  ;; Configuring project.el
  ;;
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
    (setq project-switch-commands #'project-find-file))

  )
