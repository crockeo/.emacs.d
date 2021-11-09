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
		   :olp ("todos")
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
