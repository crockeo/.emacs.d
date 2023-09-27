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
	  (ch/builtin/set-font ch/builtin/presentation-font-size))
      (progn
	(setq ch/builtin/is-presenting t)
	(ch/builtin/set-font ch/builtin/normal-font-size))))

  (ch/builtin/set-font ch/builtin/normal-font-size)

  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
	exec-path (split-string (getenv "PATH") ":")
	inhibit-startup-screen t
	read-process-output-max (* 1024 1024 4) ;; 2MB
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

  (dolist (path (get-shell-path-entries))
    (push path exec-path))

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
   redisplay-dont-pause t
   scroll-conservatively integer-width
   scroll-margin 4
   scroll-step 1
   )

  (defun ch/builtin/garbage-collect ()
    (let ((original-gc-cons-threshold gc-cons-threshold))
      (setq gc-cons-threshold (* 1024 1024)) ;; 1MB
      (garbage-collect)
      (setq gc-cons-threshold original-gc-cons-threshold)))

  (add-hook 'focus-out-hook #'ch/builtin/garbage-collect)
  (run-with-idle-timer 1 t #'ch/builtin/garbage-collect)

  (global-auto-revert-mode 1)
  (global-display-line-numbers-mode 1)
  (global-eldoc-mode -1)
  (setq eldoc-documentation-function #'ignore))
