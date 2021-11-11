;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defun ch/org/update-all-agendas ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (derived-mode-p 'org-agenda-mode)
	  (org-agenda-redo t)))))

  (defun ch/org/register-update-all-agendas ()
    (add-hook 'after-save-hook
	      #'ch/org/update-all-agendas
	      nil
	      'local))

  (defun ch/org/config ()
    (setq org-agenda-window-setup 'current-window
	  org-adapt-indentation nil
          org-hide-emphasis-markers t)

    (auto-fill-mode 0)
    (display-line-numbers-mode 0)
    (org-indent-mode 1)
    (visual-line-mode 1))

  (defun ch/org/heading-sort-order ()
    (let ((todo-keyword (nth 2 (org-heading-components))))
      `(,@(org-get-scheduled-time nil)
	,(cl-position todo-keyword
		      (cdar org-todo-keywords)
		      :test #'string-equal))))

  (defun ch/org/heading-sort-compare ()
    ;; TODO
    )

  (defun ch/org/sort-todos ()
    (interactive)
    (org-sort-entries
     nil
     ?f
     #'ch/org/heading-sort-order))

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
    :hook
    ((org-mode . ch/org/config)
     (org-mode . ch/org/register-update-all-agendas)))

  (use-package doct
    :config
    (setq org-capture-templates
	  (doct '(("Task"
		   :keys "t"
		   :file "~/home.org"
		   :prepend t
		   :olp ("todos" "scheduled")
		   :template ("* TODO %^{Description}"
			      "SCHEDULED: %^{Scheduled}t"
                              "%?"))
		  ("Note"
		   :keys "n"
		   :file "~/home.org"
		   :prepend t
		   :olp ("notes")
		   :template ("* %^{Description}"
			      "%?")))))))
