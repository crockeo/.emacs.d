;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defvar org-directory (expand-file-name "~/org"))
  (defun ch/org/ensure-org-directory ()
    (make-directory org-directory t))

  ;; these functions help me synchronize my files across devices
  ;; by just sending them to and retrieving them from a git repo!
  (defun ch/org/download-files ()
    (interactive)
    (message "org.el: Downloading files...")
    (ch/org/ensure-org-directory)
    (condition-case ()
	(progn
	  (ch/under-dir org-directory
	    (ch/callproc "git" "pull" "origin" (magit-get-current-branch)))
	  (message "org.el: Downloaded files!"))
      (error (message "org.el: Failed to download files."))))

  (defun ch/org/upload-files ()
    (interactive)
    (message "org.el: Uploading files...")
    (ch/org/ensure-org-directory)
    (condition-case ()
	(progn
	  (ch/under-dir org-directory
	    (ch/callproc "git" "add" ".")
	    (ch/callproc "git" "commit" "-m" (format "%s: syncing org files" (current-time-string)))
	    (ch/callproc "git" "push" "-u" "origin" (magit-get-current-branch)))
	  (message "org.el: Uploaded files!"))
      (error (message "org.el: Failed to upload files."))))

  ;; these functions help me go back and forth
  ;; between whatever i'm doing
  ;; and my org-roam stuff
  (defvar ch/org/winconf nil)

  (defun ch/org/save-winconf (&optional winconf)
    (setq ch/org/winconf
	  (or ch/org/winconf
	      winconf
	      (current-window-configuration))))

  (defun ch/org/pop-winconf ()
    (interactive)
    (when (not ch/org/winconf)
      (error "No prior window configuration!"))
    (set-window-configuration ch/org/winconf)
    (setq ch/org/winconf nil))

  (defmacro ch/org/declare-winconf-funcs (&rest funcs)
    `(progn
     ,@(-map
	(lambda (func)
	  `(defun ,(intern (concat "ch/org/" (symbol-name (car func)))) ()
	     (interactive)
	     (let ((winconf (current-window-configuration)))
	       (,@func)
	       (ch/org/save-winconf winconf))))
	funcs)))

  (ch/org/declare-winconf-funcs
   (org-roam-dailies-goto-today)
   (org-roam-dailies-goto-tomorrow 1)
   (org-roam-dailies-goto-yesterday 1)
   (org-roam-node-find))

  ;; and the rest of this is just plain configuration :)
  (defun ch/org/config ()
    (setq org-adapt-indentation nil
	  org-hide-emphasis-markers t)

    (auto-fill-mode -1)
    (display-line-numbers-mode -1)
    (org-indent-mode t)
    (visual-line-mode t)
    (org-overview))

  (use-package org
    :config
    (setq org-capture-bookmark nil
	  org-directory (expand-file-name "~/org")
	  org-log-time 'done
	  org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE"))
	  org-todo-keyword-faces `(("TODO" . ,(ch/zenburn/color "red"))
				   ("IN-PROGRESS" . ,(ch/zenburn/color "yellow"))
				   ("WAITING" . ,(ch/zenburn/color "bg+3"))
				   ("DONE" . ,(ch/zenburn/color "green"))))
    :hook (org-mode . ch/org/config)
    :bind (:map org-mode-map
		("C-c o i" . org-roam-node-insert)))

  (use-package org-bullets
    :after org
    :hook ((org-mode . org-bullets-mode)))

  (use-package org-roam
    :after org
    :config
    (setq
     org-roam-directory org-directory
     org-roam-dailies-directory "dailies/"
     org-roam-dailies-capture-templates '(("d" "default" entry
					   "* %?"
					   :target (file+head "%<%Y-%m-%d>.org"
							      "#+title: %<%Y-%m-%d>\n"))))

    (org-roam-db-autosync-mode t))

  ;; org-roam-dailies isn't loaded by default
  ;; under straight.el for some reason.
  ;; manually load it here instead
  (eval-after-load 'org-roam
    (progn
      (add-to-list 'load-path (expand-file-name "~/.emacs.d/straight/repos/org-roam/extensions"))
      (load "org-roam-dailies"))))
