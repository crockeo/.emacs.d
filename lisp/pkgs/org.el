;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defvar ch/org/directory
    (file-name-as-directory (expand-file-name "~/org")))

  (use-package org
    :config
    (setq org-default-notes-file (concat ch/org/directory "inbox.org"))
    (setq org-capture-bookmark nil)
    (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "DONE")))
    (setq org-log-done 'time))

  (use-package org-contrib
    :after org)

  (use-package org-modern
    :after org
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-label-border 1)
    (setq org-modern-todo-faces
	  '(("TODO"
	     :weight bold
	     :foreground "gray30"
	     :background "gray90")
	    ("NEXT"
	     :weight bold
	     :foreground "white"
	     :background "DodgerBlue1")
	    ("WAITING"
	     :weight bold
	     :foreground "white"
	     :background "gray50")
	    ("DONE"
	     :weight bold
	     :foreground "white"
	     :background "green4"))))

  (use-package org-roam
    :after org
    :bind
    ("C-c o i" . org-roam-node-insert)
    ("C-c o t" . ch/org/add-filetags)
    :config
    (setq org-roam-directory ch/org/directory)
    (org-roam-db-autosync-mode 1))

  (use-package org-transclusion
    :after org)

  (defun ch/org/config ()
    (diff-hl-mode -1)
    (display-line-numbers-mode -1)
    (org-indent-mode))

  (add-hook 'org-mode-hook #'ch/org/config)

  (defun ch/org/capture ()
    (interactive)
    (org-capture nil "t"))

  (defun ch/org/add-filetags ()
    (interactive)
    (when-let ((tags (call-interactively #'org-roam-tag-add)))
      (ch/org/ensure-filetags tags)))

  (defun ch/org/roam-node-predicate (pos-tags neg-tags)
    ;; Constructs a predicate that requires an org roam node
    ;; has all of the required `pos-tags`
    ;; and has none of the `neg-tags`.
    (lambda (node)
      (let ((tags (org-roam-node-tags node)))
	(and (-all? (-partial #'seq-contains-p tags) pos-tags)
	     (not (-any? (-partial #'seq-contains-p tags) neg-tags))))))

  (defvar ch/org/go-blocklist
    '("archive" "backlog" "done"))

  (defmacro ch/org/go (&rest body)
    (declare (indent defun))
    `(let ((winconf (current-window-configuration)))
       (condition-case nil
	   (progn
	     ,@body
	     (delete-other-windows)
	     (ch/winconf/save winconf))
	 (error (ch/winconf/pop winconf)))))

  (defun ch/org/go-home ()
    (interactive)
    (ch/org/go
      (find-file (expand-file-name "~/org/home.org"))))

  (defun ch/org/go-node ()
    (interactive)
    (ch/org/go
      (org-roam-node-find
       nil nil
       (ch/org/roam-node-predicate nil ch/org/go-blocklist))))

  (defun ch/org/go-project ()
    (interactive)
    (ch/org/go
      (org-roam-node-find
       nil nil
       (ch/org/roam-node-predicate '("project") ch/org/go-blocklist))))
  )
