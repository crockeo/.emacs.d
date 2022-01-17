;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defvar org-directory (expand-file-name "~/org"))
  (defun ch/org/ensure-org-directory ()
    (make-directory org-directory t))

  ;; sometimes i want to ping myself about my org config
  ;; while i'm not actually on a computer
  ;; these help me export my org TODOs to macOS reminders
  (defun ch/org/make-reminder/has-time (time)
    (pcase (decode-time time)
      (`(,seconds ,minutes ,hours ,_ ,_ ,_ ,_, _ ,_)
       (not (= (+ seconds minutes hours) 0)))))

  (defun ch/org/make-reminder/format-time (time)
    (format-time-string
     (concat "%A, %B %d, %Y at "
	     (if (ch/org/make-reminder/has-time time)
		 "%I:%M:%S %p"
	       "09:00:00 AM"))
     time))

  ;; a good reference:
  ;; https://forum.latenightsw.com/t/create-a-reminder/1001/2
  ;;
  ;; some TODOs here:
  ;;   - make a better system around quoting,
  ;;     so i don't have to have (format) calls here
  ;;
  ;;   - make the scheduled time smarter, so that:
  ;;     - if there is no time present, notify at 9am
  ;;     - if there is no "SCHEDULED" present,
  ;;       find out what day it is from the org-roam heading
  ;;     - if there is no org-roam heading,
  ;;       just make a reminder without a due date
  ;;
  ;;   - trim the irrelevant contents of a headline
  ;;     (e.g. "SCHEDULED" and properties)
  ;;     to reduce noise in the TODOs i create
  (defun ch/org/make-reminder/make-command ()
    (let* ((subtree (org-ml-parse-this-subtree))
	   (title (format "\"%s\"" (org-ml-get-property :raw-value subtree)))

	   (scheduled-timestamp (org-element-property :scheduled subtree))
	   (scheduled (format "date \"%s\""
			      (ch/org/make-reminder/format-time (org-timestamp-to-time scheduled-timestamp))))

	   (body (format "\"%s\"" (buffer-substring (org-ml-get-property :contents-begin subtree)
						    (org-ml-get-property :contents-end subtree)))))
      (applescript-command
       (:tell "\"Reminders\""
	      (:set "existingReminder"
		    (: "reminders where"
		       (:and (:= "name" title)
			     (:= "due date" scheduled)
			     (:= "body" body))))
	      (:if (:= "existingReminder" (:dict))
		   (: "make new reminder with properties"
		      (:dict ("name" . title)
			     ("body" . body)
			     ("due date" . scheduled))))))))

  (defun ch/org/make-reminder ()
    (interactive)
    (do-applescript (ch/org/make-reminder/make-command)))

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
   (org-roam-dailies-goto-date)
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
		("C-c o r" . ch/org/make-reminder)
		("C-c o i" . org-roam-node-insert)))

  (use-package org-bullets
    :after org
    :hook ((org-mode . org-bullets-mode)))

  (use-package org-ml
    :after org)

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
