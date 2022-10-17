;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defvar ch/org/directory (file-name-as-directory (expand-file-name "~/org")))
  (setq org-directory ch/org/directory)

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

  (use-package org-ql
    :after org)

  (use-package org-roam
    :after org
    :bind
    ("C-c o i" . org-roam-node-insert)
    ("C-c o t" . ch/org/add-filetags)
    :config
    (setq org-roam-directory ch/org/directory)
    (org-roam-db-autosync-mode 1))

  (use-package org-super-agenda
    :after org
    :config
    (setq org-super-agenda-groups
	  '((:auto-map (lambda (item) (ch/org/category)))))
    :hook (org-agenda-mode . org-super-agenda-mode))

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

  (defun ch/org/get-buffer-prop (name)
    ;; shamelessly stolen from
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
    `(let ((winconf (current-window-configuration))
	   (error nil))
       (unwind-protect
	   (progn
	     ,@body
	     (delete-other-windows)
	     (ch/winconf/save winconf))
	 (when error
	   (ch/winconf/pop winconf)
	   (message "%s" error)))))

  (defun ch/org/files ()
    (--> (directory-files ch/org/directory)
      (-filter (lambda (name) (string-match-p (regexp-quote ".org") name)) it)
      (-map (lambda (name) (concat ch/org/directory name)) it)))

  ;;
  ;; Step 1. Temporal!
  ;;
  ;; - Inbox = things I haven't looked at yet
  ;; - Today = things I should do today
  ;; - Someday = things that can be done any time
  ;; - Oneday = things that I'm putting off
  ;; - Logbook = things which have been done recently
  ;;
  (defun ch/org/go-inbox ()
    (interactive)
    (ch/org/go
      (find-file (concat ch/org/directory "inbox.org"))))

  (defun ch/org/go-today ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	'(and (ts-active :on today) (todo))
	:title "Today"
	:super-groups '((:auto-map (lambda (item) (ch/org/category)))))))

  (defun ch/org/go-someday ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	'(and (todo)
	      (not (ts-active))
	      (not (tags "oneday")))
	:title "Someday"
	:super-groups '((:auto-map (lambda (item) (ch/org/category)))))))

  (defun ch/org/go-oneday ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	'(and (todo)
	      (not (ts-active))
	      (tags "oneday"))
	:title "Oneday"
	:super-groups '((:auto-map (lambda (item) (ch/org/category)))))))

  (defun ch/org/go-logbook ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	'(and (done))
	:title "Inbox"
	:sort 'date ;; TODO: descending?
	)
      ))

  (ch/crockeo/register-keys
    ("C-c C-w C-i" . ch/org/go-inbox)
    ("C-c C-w C-t" . ch/org/go-today)
    ("C-c C-w C-s" . ch/org/go-someday)
    ("C-c C-w C-o" . ch/org/go-oneday)
    ("C-c C-w C-l" . ch/org/go-logbook)
    ))
