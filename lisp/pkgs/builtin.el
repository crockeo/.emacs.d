;;; load-pkgs.el -*- lexical-binding: t; -*-

(ch/pkg builtin
  (add-hook 'before-save-hook #'delete-trailing-whitespace)

  (let ((font-face "Menlo")
	(font-size 120))
   (condition-case nil
       (set-face-attribute 'default nil
			   :font font-face
			   :height font-size)
     (error
      (set-face-attribute 'default nil
			  :height font-size))))

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
  (setq eldoc-documentation-function #'ignore)

  ;; tabs!!!!!!
  ;; TODO: move this over to evil.el(?)
  (dolist (num '(1 2 3 4 5 6 7 8))
    (let ((num num))
     (global-set-key
      (kbd (format "s-%d" num))
      (lambda () (interactive) (tab-bar-select-tab num)))))

  (global-set-key (kbd "s-9") #'last-tab)
  (global-set-key (kbd "s-t") #'tab-new)
  (global-set-key (kbd "s-w") #'tab-close))
