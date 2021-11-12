;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  ;; for some reason i'm getting a nil defun issue
  ;; when attempting to close emacs
  ;; defining an empty function for org-clocking-buffer
  ;; seems to solve the problem
  (defun org-clocking-buffer ())

  (defun ch/org/update-all-agendas ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (derived-mode-p 'org-agenda-mode)
	  (org-agenda-redo t)))))

  (defun ch/org/config ()
    (setq org-agenda-window-setup 'current-window
	  org-adapt-indentation nil
          org-hide-emphasis-markers t)

    (auto-fill-mode 0)
    (display-line-numbers-mode 0)
    (org-indent-mode 1)
    (visual-line-mode 1)

    (add-hook 'after-save-hook
	      #'ch/org/update-all-agendas
	      nil
	      'local))

  (defun ch/org/todo-sort/order ()
    ;; provides a multi-layered sort order for TODOs such that:
    ;;
    ;; - items scheduled before other items are listed first
    ;; - items due before other items are listed first
    ;; - items in a more actionable state are listed first
    ;;
    ;; in that order
    (let ((todo-keyword (nth 2 (org-heading-components))))
      `(,@(org-get-scheduled-time nil)
	,@(org-get-deadline-time nil)
	,(cl-position todo-keyword
		      (cdar org-todo-keywords)
		      :test #'string-equal))))

  (defun ch/org/todo-sort/cmp (order1 order2)
    (cl-loop for x in order1
	     for y in order2
	     if (< x y) return t
	     if (> x y) return nil))

  (defun ch/org/todo-sort/sort ()
    (org-sort-entries
     nil
     ?f
     #'ch/org/todo-sort/order
     #'ch/org/todo-sort/cmp))

  (defun ch/org/todo-sort/elements-on-olp (olp org-headlines)
    (let* ((head (car org-headlines))
	   (head-text (org-element-property :raw-value head)))
     (cond
      ;; if either olp or org-headlines is nil
      ;; then we've find our element,
      ;; or exhausted our search, respectively
      ((or (null olp) (null org-headlines))
       nil)

      ;; TODO: account for the fact that we could be beyond the span
      ;; doesn't matter yet, but will if i move my org around
      ((string-equal head-text (car olp))
       (cons head (ch/org/todo-sort/elements-on-olp (cdr olp) (cdr org-headlines))))

      ;; otherwise we just keep searching with remaining arguments :)
      (t
       (ch/org/todo-sort/elements-on-olp olp (cdr org-headlines))))))

  (defun ch/org/todo-sort/position (olp)
    (let* ((elements (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity))
	   (elements-on-olp (ch/org/todo-sort/elements-on-olp olp elements)))
      (org-element-property :begin (car (last elements-on-olp)))))

  (defun ch/org/todo-sort/todo-sort (olp)
    (goto-char (ch/org/todo-sort/position olp)))

  (defun ch/org/todo-sort ()
    (interactive)
    (with-current-buffer (find-file-noselect "~/home.org")
      (ch/org/todo-sort/todo-sort '("todos" "scheduled"))
      (ch/org/todo-sort/sort)
      ;; org-sort always puts the headline into SUBTREE
      ;; so cycling twice brings us to CHILDREN,
      ;; which we want.
      ;; TODO: better way of doing this
      (org-cycle)
      (org-cycle)))

  (defun ch/org/capture-hook ()
    (ch/org/todo-sort))

  (add-hook 'org-capture-after-finalize-hook #'ch/org/capture-hook)

  (defvar ch/org/home/window-configuration nil)

  (defmacro ch/org/home/toggle (&rest body)
    (declare (indent defun))
    `(if ch/org/home/window-configuration
	 (progn
	   (set-window-configuration ch/org/home/window-configuration)
	   (setq ch/org/home/window-configuration nil))
       (progn
	 (setq ch/org/home/window-configuration (current-window-configuration))
	 ,@body)))

  (defun ch/org/home/go-home ()
    (interactive)
    (ch/org/home/toggle
      (delete-other-windows)
      (find-file "~/home.org")
      (let ((home-window (get-buffer-window))
	    (agenda-window (split-window-horizontally)))
	(select-window agenda-window)
	(org-agenda-list)
	(org-agenda-day-view)
	(select-window home-window))))

  (defun ch/org/home/go-week ()
    (interactive)
    (ch/org/home/toggle
      (org-agenda-list)))

  (defun ch/org/home/go-today ()
    (interactive)
    (ch/org/home/toggle
      (org-agenda-list)
      (org-agenda-day-view)))

  (defun ch/org/home/go-back ()
    (interactive)
    (if ch/org/home/window-configuration
	(progn
	  (set-window-configuration ch/org/home/window-configuration)
	  (setq ch/org/home/window-configuration nil))
      (message "No prior window configuration.")))

  (use-package org
    :config
    ;; TODO: make this prettier :)
    (setq org-agenda-skip-scheduled-if-done t)
    (setq org-capture-bookmark nil)
    (setq org-todo-keywords '((sequence "TODO" "NEEDS-REVIEW" "WAITING" "|" "DONE")))
    (setq org-log-done 'time)
    (progn
      (require 'hawaii-theme)
      (setq org-todo-keyword-faces
	    `(("TODO" . org-warning)
	      ("NEEDS-REVIEW" . ,hawaii-highlight-blue)
	      ("WAITING" . ,hawaii-comment)
	      ("DONE" . ,hawaii-highlight-green))))

    :bind
    (:map global-map
     ("C-c w h" . ch/org/home/go-home)
     ("C-c w w" . ch/org/home/go-week)
     ("C-c w t" . ch/org/home/go-today)
     ("C-c w b" . ch/org/home/go-back)

     ;; TODO: figure out binding to org-agenda work
     )

    :hook
    ((org-mode . ch/org/config)
     (org-mode . ch/org/register-update-all-agendas)))

  ;; (1) filing TODOS in the OLP structure ("todos" "<date of scheduling>")
  ;;
  ;; - info on how to file to date tree
  ;;   https://emacs.stackexchange.com/questions/12576/is-it-possible-to-file-a-capture-template-in-a-subheading-of-a-day-in-a-datetree
  ;;
  ;; (2) providing a function to reschedule a headline AND to move it to the appropriate OLP of ("todos" "<date of scheduling">)
  ;;
  ;; (3) marking items as done and refiling them to complete
  ;;
  ;; alternate approach: ignore the actual org headlines entirely
  ;; and use org-search and org-agenda for my organization

  (use-package doct
    :config
    (setq org-capture-templates
	  (doct '(("Task"
		   :keys "t"
		   :file "~/home.org"
		   :olp ("todos" "scheduled")
		   :template ("* TODO %^{Description}"
			      "SCHEDULED: %^{Scheduled}t"
                              "%?"
			      ""))
		  ("Note"
		   :keys "n"
		   :file "~/home.org"
		   :prepend t
		   :olp ("notes")
		   :template ("* %^{Description}"
			      "%?"
			      "")))))))
