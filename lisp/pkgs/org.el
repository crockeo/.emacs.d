;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  ;; for some reason i'm getting a nil defun issue
  ;; when attempting to close emacs
  ;; defining an empty function for org-clocking-buffer
  ;; seems to solve the problem
  (defun org-clocking-buffer ())

  (defvar ch/org/org-directory
    (let ((path (expand-file-name "~/org")))
      (unless (file-exists-p path)
	(make-directory path))
      (file-truename path)))

  (defvar ch/org/inbox-file
    (concat (file-name-as-directory ch/org/org-directory)
	    "inbox"
	    "-"
	    (system-name)
	    ".org"))

  ;; these functions provide interfaces to org headlines
  ;; as though i had the files themselves open.
  ;; i like this as a means to interact with content
  ;; through org-agenda, instead of having to open full files
  (defmacro ch/org/with-headline (reload &rest body)
    (declare (indent defun))
    `(let* ((marker (org-get-at-bol 'org-marker))
	    (buffer (marker-buffer marker))
	    (pos (marker-position marker)))
       (save-excursion
	 (with-current-buffer buffer
	   (goto-char pos)
	   ,@body)
	 ,@(when reload
	     ;; most buffers bind some kind of "reload" to 'g'.
	     ;; this disgusting little hack leverages that
	     ;; to dynamically call the current buffer's reload
	     '((funcall (lookup-key (current-local-map) "g")))))))

  (defun ch/org/agenda-refile ()
    (interactive)
    (ch/org/with-headline t
      (org-roam-refile)
      (save-buffer)))

  (defun ch/org/agenda-goto ()
    (interactive)
    (ch/org/with-headline nil
      (org-tree-to-indirect-buffer)
      (other-window 1)))

  (defun ch/org/agenda-next ()
    (interactive)
    (ch/org/with-headline t
      (ch/org/set-next)))

  (defun ch/org/set-next ()
    (interactive)
    (save-excursion
      (ignore-errors
	(outline-back-to-heading)
	(org-set-tags (seq-uniq (cons "next" (org-get-tags)))))))

  (defun ch/org/quit-indirect-buffer ()
    (interactive)
    (let ((is-indirect-buffer (buffer-base-buffer))
	  (window-count (length (window-list))))
      (when (and is-indirect-buffer
		 (> window-count 1))
	(delete-window))))

  (defun ch/org/get-buffer-prop (name)
    ;; taken from:
    ;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
    (org-with-point-at 1
      (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                               (point-max) t)
	(buffer-substring-no-properties
	 (match-beginning 1)
	 (match-end 1)))))

  (defun ch/org/category (&optional max-length)
    ;; inspired
    ;; https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
    (let* ((title (ch/org/get-buffer-prop "title"))
	   (category (org-get-category))
	   (file-name (when buffer-file-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
	   (result (or title category file-name "")))
      (if (and max-length
	       (> (length result) max-length))
	  (concat (substring result 0 (- max-length 3)) "...")
	result)))

  (defun ch/org/get-filetags ()
    (-filter
     (-compose #'not #'string-empty-p)
     (split-string (ch/org/get-buffer-prop "filetags") ":")))

  (defun ch/org/title-to-tag (title)
    (->> title
      (string-replace "-" "_")
      (string-replace " " "_")))

  (defun ch/org/ensure-filetags (new-tags)
    (let ((title (ch/org/get-buffer-prop "title"))
	  (tags (ch/org/get-filetags)))
      (when (seq-contains new-tags "person")
	(org-roam-tag-add (list (concat "@" (ch/org/title-to-tag title)))))))

  (defun ch/org/add-filetags ()
    (interactive)
    (let ((tags (call-interactively #'org-roam-tag-add)))
      (when tags
	(ch/org/ensure-filetags tags))))

  ;; i interact with my org files primarily through various views
  ;; built on top of data that is stored inside of org-roam nodes
  ;; these functions define:
  ;;
  ;; - how to go to each of the views
  ;; - how to return to the prior window configuration
  (defvar ch/org/winconf nil)

  (defun ch/org/save-winconf (&optional winconf)
    (unless ch/org/winconf
      (setq ch/org/winconf (or winconf (current-window-configuration)))))

  (defun ch/org/pop-winconf ()
    (interactive)
    (if ch/org/winconf
	(progn
	  (set-window-configuration ch/org/winconf)
	  (setq ch/org/winconf nil)
	  (org-save-all-org-buffers))
      (message "No prior window configuration.")))

  (defun ch/org/search (title query)
    (ch/org/save-winconf)
    (org-ql-search
      #'org-agenda-files
      query
      :title title
      :super-groups '((:auto-map (lambda (item) (ch/org/category)))))
    (delete-other-windows))

  (defun ch/org/go-week ()
    (interactive)
    (ch/org/save-winconf)
    (org-agenda-list))

  (defun ch/org/go-day ()
    (interactive)
    (ch/org/save-winconf)
    (org-agenda-list)
    (org-agenda-day-view))

  (defun ch/org/go-recent ()
    (interactive)
    (ch/org/save-winconf)
    (org-ql-view-recent-items
     :num-days 7
     :type 'closed
     :groups '((:auto-map (lambda (item) (ch/org/category)))))
    (delete-other-windows))

  (defun ch/org/go-yesterday ()
    (interactive)
    (ch/org/save-winconf)
    (org-ql-view-recent-items
     :num-days 1
     :type 'closed
     :groups '((:auto-map (lambda (item) (ch/org/category)))))
    (delete-other-windows))

  (defun ch/org/go-backlog ()
    (interactive)
    (ch/org/search
     "Backlog"
     '(and (todo)
	   (not (ts-active))
	   (not (tags "refile")))))

  (defun ch/org/go-next ()
    (interactive)
    (ch/org/search
     "Next"
     '(and (not (done)) (tags "next"))))

  (defun ch/org/go-inbox ()
    (interactive)
    (ch/org/search
     "Inbox"
     '(tags "refile")))

  (defun ch/org/go-roam-find ()
    (interactive)
    (let ((winconf (current-window-configuration)))
      (condition-case ()
	  (progn
	    (org-roam-node-find)
	    (ch/org/save-winconf winconf))
	(quit ()))))

  (defun ch/org/agenda-quit ()
    (interactive)
    (if ch/org/winconf
	(ch/org/pop-winconf)
      (org-agenda-quit)))

  ;; not sure why, but some config items must be set
  ;; after loading org-mode / org-agenda-mode.
  ;; these functions provide that configuration
  (defun ch/org/config ()
    (setq org-adapt-indentation nil
          org-hide-emphasis-markers t)

    (auto-fill-mode 0)
    (display-line-numbers-mode 0)
    (org-indent-mode 1)
    (visual-line-mode 1))

  (defun ch/org/config-agenda ()
    (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s")
				     (todo . " %i ")
				     (tags . " %i ")
				     (search . " %i ")))

    (setq org-agenda-sorting-strategy
	  '((agenda todo-state-up habit-down time-up priority-down)
	    (todo todo-state-up priority-down category-keep)
	    (tags todo-state-up priority-down category-keep)
	    (search todo-state-up category-keep)))

    (define-key org-agenda-mode-map (kbd "C-c o r") #'ch/org/agenda-refile)
    (define-key org-agenda-mode-map (kbd "C-c o n") #'ch/org/agenda-next)
    (org-remap org-agenda-mode-map
	       'bury-buffer 'ch/org/agenda-quit
	       'org-agenda-goto 'ch/org/agenda-goto
	       'org-agenda-quit 'ch/org/agenda-quit))

  (use-package org
    :config
    (setq org-agenda-files (list ch/org/org-directory)
	  org-agenda-skip-scheduled-if-done t
	  org-agenda-scheduled-leaders '("" "")
	  org-agenda-use-time-grid nil
	  org-agenda-window-setup 'current-window
	  org-capture-bookmark nil
	  org-directory ch/org/org-directory
	  org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "NEEDS-REVIEW" "WAITING" "|" "DONE"))
	  org-log-done 'time
	  org-todo-keyword-faces `(("TODO" . ,(ch/zenburn/color "red"))
				   ("IN-PROGRESS" . ,(ch/zenburn/color "yellow"))
				   ("NEEDS-REVIEW" . ,(ch/zenburn/color "blue"))
				   ("WAITING" . ,(ch/zenburn/color "bg+3"))
				   ("DONE" . ,(ch/zenburn/color "green"))))

    :hook
    ((org-mode . ch/org/config)
     (org-agenda-mode . ch/org/config-agenda)))

  (use-package org-bullets
    :after org
    :hook
    ((org-mode . org-bullets-mode)))

  (use-package org-ml
    :after org)

  (use-package org-ql
    :after org)

  (use-package org-roam
    :after org
    :config
    (setq org-roam-directory ch/org/org-directory)
    (org-roam-db-autosync-mode))

  (use-package org-super-agenda
    :after org
    :config
    (setq org-super-agenda-groups '((:auto-map (lambda (item) (ch/org/category)))))
    :hook ((org-agenda-mode . org-super-agenda-mode)))

  (use-package doct
    :after org
    :config
    (setq org-capture-templates
	  (doct '(("Task"
		   :keys "t"
		   :file ch/org/inbox-file
		   :template ("* TODO %^{Description}"
                              "%?"))

		  ("Note"
		   :keys "n"
		   :file ch/org/inbox-file
		   :template ("* %^{Description}"
			      "%?")))))))
