;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defvar org-directory (expand-file-name "~/org"))
  (defun ch/org/ensure-org-directory ()
    (make-directory org-directory t))

  (defun ch/org/ensure-filetags (new-tags))

  (defun ch/org/add-filetags ()
    (interactive)
    (when-let ((tags (call-interactively #'org-roam-tag-add)))
      (ch/org/ensure-filetags tags)))

  ;; sometimes i want to ping myself about my org config
  ;; while i'm not actually on a computer
  ;; these help me export my org TODOs to macOS reminders
  (defun ch/org/make-reminder/get-prop (name)
    ;; shamelessly stolen from
    ;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
	(buffer-substring-no-properties
	 (match-beginning 1)
	 (match-end 1)))))

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

  (defun ch/org/make-reminder/parse-daily-title ()
    "Parses the #+title: ... property of an org-mode file \
into its encoded time equivalent at 9:00am."
    (if-let* ((title (ch/org/make-reminder/get-prop "title"))
	      (time (parse-time-string title)))
	(pcase (parse-time-string title)
	  (`(,_ ,_ ,_ ,day ,month ,year ,dow ,dst ,utcoff)
	   (when (and day month year)
	    (encode-time (list 0 0 9 day month year dow dst utcoff)))))))

  (cl-defun ch/org/make-reminder/get-scheduled (subtree)
    ;; order of preference here is:
    ;;
    ;; 1. org headline SCHEDULED time
    ;; 2. org-roam-dailies day at 9:00am
    ;; 3. no due date
    (when-let ((scheduled (org-element-property :scheduled subtree)))
      (cl-return-from ch/org/make-reminder/get-scheduled
	(ch/org/make-reminder/format-time (org-timestamp-to-time scheduled))))

    (when-let* ((title-scheduled (ch/org/make-reminder/parse-daily-title)))
      (cl-return-from ch/org/make-reminder/get-scheduled
	(ch/org/make-reminder/format-time title-scheduled)))

    ;; TODO: make other functions
    ;; more resilient to missing scheduled date
    (error "Unable to determine due date for reminder."))

  ;; a good reference:
  ;; https://forum.latenightsw.com/t/create-a-reminder/1001/2
  ;;
  ;; some TODOs here:
  ;;   - make a better system around quoting,
  ;;     so i don't have to have (format) calls here
  ;;
  ;;   - trim the irrelevant contents of a headline
  ;;     (e.g. "SCHEDULED" and properties)
  ;;     to reduce noise in the TODOs i create
  (defun ch/org/make-reminder/make-command ()
    (let* ((subtree (org-ml-parse-this-subtree))
	   (title (format "\"%s\"" (org-ml-get-property :raw-value subtree)))

	   (scheduled (format "date \"%s\"" (ch/org/make-reminder/get-scheduled subtree)))

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
  (defun ch/org/sync-files ()
    (interactive)
    (message "org.el: Syncing files...")
    (condition-case ()
	(ch/under-dir org-directory
	  (org-save-all-org-buffers)
	  (ch/callproc
	   ("git" "stash")
	   ("git" "pull" "origin" (magit-get-current-branch))
	   ("git" "stash" "pop")
	   ("git" "add" ".")
	   ("git" "commit" "-m" (format "%s: syncing org files" (current-time-string)))
	   ("git" "push" "-u" "origin" (magit-get-current-branch)))
	  (message "org.el: Synced files!"))
      (error (message "org.el: Failed to upload files."))))

  (defun ch/org/download-files ()
    (interactive)
    (message "org.el: Downloading files...")
    (ch/org/ensure-org-directory)
    (condition-case ()
	(progn
	  (ch/under-dir org-directory
	    (ch/callproc ("git" "pull" "origin" (magit-get-current-branch))))
	  (message "org.el: Downloaded files!"))
      (error (message "org.el: Failed to download files."))))

  (defun ch/org/upload-files ()
    (interactive)
    (message "org.el: Uploading files...")
    (ch/org/ensure-org-directory)
    (condition-case ()
	(progn
	  (ch/under-dir org-directory
	    (ch/callproc
	     ("git" "add" ".")
	     ("git" "commit" "-m" (format "%s: syncing org files" (current-time-string)))
	     ("git" "push" "-u" "origin" (magit-get-current-branch))))
	  (message "org.el: Uploaded files!"))
      (error (message "org.el: Failed to upload files."))))

  (run-with-idle-timer
   (* 60 10) ;; 10 minutes
   t
   #'ch/org/sync-files)

  (dolist (hook '(emacs-startup-hook kill-emacs-hook))
    (add-hook hook #'ch/org/sync-files))

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
	  org-log-done 'time
	  org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE"))
	  org-todo-keyword-faces `(("TODO" . ,(ch/zenburn/color "red"))
				   ("IN-PROGRESS" . ,(ch/zenburn/color "yellow"))
				   ("WAITING" . ,(ch/zenburn/color "bg+3"))
				   ("DONE" . ,(ch/zenburn/color "green"))))
    :hook (org-mode . ch/org/config)
    :bind (:map org-mode-map
		("C-c o i" . org-roam-node-insert)
		("C-c o n" . org-id-get-create)
		("C-c o r" . ch/org/make-reminder)
		("C-c o s" . org-save-all-org-buffers)
		("C-c o t" . ch/org/add-filetags)))

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
