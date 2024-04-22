;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defvar ch/org/directory
    (file-name-as-directory (expand-file-name "~/org")))

  (defun ch/org/files ()
    (--> (directory-files ch/org/directory)
      (-filter (lambda (name) (string-match-p (regexp-quote ".org") name)) it)
      (-map (lambda (name) (concat ch/org/directory name)) it)))

  (defun ch/org/cycle (&rest args)
    (interactive)
    (unless (org-at-heading-or-item-p)
      (org-previous-visible-heading 1))
    (org-cycle))

  (setq org-agenda-files (ch/org/files))

  (use-package org
    :custom
    (org-agenda-hide-tags-regexp ".*")
    (org-agenda-tags-column 0)
    (org-agenda-prefix-format
     '((agenda . "  ")
       (todo . "  ")
       (tags . "  ")
       (search . " ")))
    (org-agenda-format-date
     (lambda (date)
       (concat
	"\n\n"
	(org-agenda-format-date-aligned date))))
    (org-agenda-restore-windows-after-quit t)
    (org-auto-align-tags nil)
    (org-capture-bookmark nil)
    (org-default-notes-file (concat ch/org/directory "inbox.org"))
    (org-directory ch/org/directory)
    (org-element-use-cache nil)
    (org-hide-emphasis-markers t)
    (org-link-descriptive t)
    (org-log-done 'time)
    (org-log-repeat nil)
    (org-startup-folded 'content)
    (org-startup-truncated nil)
    (org-tags-column 0)
    (org-tags-exclude-from-inheritance '("project" "area" "reference"))
    (org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "DONE")))
    (org-todo-keyword-faces '(("TODO" . org-todo)
			      ("NEXT" . org-warning)
			      ("WAITING" . org-agenda-dimmed-todo-face)
			      ("DONE" . org-done)))

    :custom-face
    (org-code ((t (:background ,(modus-themes-get-color-value 'bg-inactive)))))
    (org-done ((t (:foreground ,(modus-themes-get-color-value 'bg-active)))))
    (org-done-headline ((t (:foreground ,(modus-themes-get-color-value 'bg-active)))))
    (org-level-1 ((t (:height 1.15))))
    (org-level-2 ((t (:height 1.10))))
    (org-level-3 ((t (:heihgt 1.05))))

    :config
    (evil-define-key 'normal org-mode-map
      (kbd "<tab>") #'ch/org/cycle)

    (setq org-capture-templates
	  `(("t" "TODO" entry (file+headline ,(concat ch/org/directory "inbox.org") "Tasks")
	     "* TODO %?\n"))))

  (use-package org-autolist
    :after org
    :hook (org-mode . org-autolist-mode))

  (use-package org-contrib
    :after org)

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

  (defun ch/org/config ()
    (diff-hl-mode -1)
    (display-line-numbers-mode -1)
    (visual-line-mode t)
    (org-indent-mode))

  (defun ch/org/config-agenda ()
    (diff-hl-mode -1)
    (display-line-numbers-mode -1)
    (visual-line-mode t))

  (add-hook 'org-mode-hook #'ch/org/config)
  (add-hook 'org-agenda-finalize-hook #'ch/org/config-agenda)

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
    `(if ch/winconf/state
	 (progn ,@body)
       (let ((winconf (current-window-configuration))
	     (error nil))
	 (unwind-protect
	     (progn
	       ,@body
	       (delete-other-windows)
	       (ch/winconf/save winconf))
	   (when error
	     (ch/winconf/pop winconf)
	     (message "%s" error))))))

  ;;
  ;; Step 1. Temporal!
  ;;
  ;; When do I need to do something
  ;; or when did I do something?
  ;;
  ;; - Today = things I should do today
  ;; - Someday = things that can be done any time
  ;; - Oneday = things that I'm putting off
  ;; - Logbook = things which have been done recently
  ;;
  (defvar test-heading nil)

  (defun ch/org/todo-sort/todo-keyword (headline)
    (let* ((todo-keyword (org-element-property :todo-keyword headline))
	   (start 0)
	   (end (length todo-keyword)))
      (set-text-properties start end nil todo-keyword)
      todo-keyword))

  (defun ch/org/todo-sort/todo-keyword-cmp (headline)
    (let ((todo-keyword (ch/org/todo-sort/todo-keyword headline)))
      (pcase todo-keyword
	("NEXT" 0)
	("TODO" 1)
	("WAITING" 2)
	("DONE" 3)
	(_ (error "Invalid todo-keyword. Is there a naked TODO without a headline?")))))

  (defun ch/org/todo-sort/cmp-todo-keyword (headline1 headline2)
    (let ((headline1-cmp (ch/org/todo-sort/todo-keyword-cmp headline1))
	  (headline2-cmp (ch/org/todo-sort/todo-keyword-cmp headline2)))
      (pcase (- headline1-cmp headline2-cmp)
	((pred (< 0)) -1)
	((pred (> 0)) 1)
	(0 0))))

  (defun ch/org/todo-sort (headline1 headline2)
    (cl-block "todo-sort"
      (cl-ecase (ch/org/todo-sort/cmp-todo-keyword headline1 headline2)
	(-1 (cl-return-from "todo-sort" nil))
	(1 (cl-return-from "todo-sort" t))
	(0 nil))))

  (defun ch/org/go-today ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	'(and (todo)
	      (or (scheduled :to today)
		  (deadline :to today)))
	:super-groups '((:auto-map (lambda (item) (ch/org/category))))
	:sort #'ch/org/todo-sort)))

  (defun ch/org/go-upcoming ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	'(and (todo)
	      (or (scheduled) (deadline)))
	:super-groups '((:auto-ts))
	:sort #'ch/org/todo-sort)))

  (defun ch/org/go-anytime ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	`(and (todo)
	      (not (scheduled))
	      (not (deadline))
	      (not (tags "oneday"))
	      (not (ancestors (todo))))
	:title "Anytime"
	:super-groups '((:auto-map (lambda (item) (ch/org/category))))
	:sort #'ch/org/todo-sort)))

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
	'(done)
	:title "Logbook"
	:sort '(closed reverse)
	:super-groups '((:auto-ts reverse)))))

  ;;
  ;; Step 2. Spatial
  ;;
  ;; Where do I put things?
  ;;
  ;; - Inbox = things I haven't looked at yet
  ;; - Areas = collections of multiple projects and tasks
  ;; - Projects = collections of multiple tasks
  (defun ch/org/capture ()
    (interactive)
    (ch/org/go
      (org-capture nil "t")))

  (defun ch/org/go-find-node ()
    (interactive)
    (ch/org/go
      (org-roam-node-find
       nil
       ""
       (ch/org/roam-node-predicate
	nil
	ch/org/go-blocklist))))

  (defun ch/org/go-find-project ()
    (interactive)
    (ch/org/go
      (org-roam-node-find
       nil
       ""
       (ch/org/roam-node-predicate
	'("project")
	`("oneday" ,@ch/org/go-blocklist)))))

  (defun ch/org/go-find-knowledge ()
    (interactive)
    (ch/org/go
      (org-roam-node-find
       nil
       ""
       (ch/org/roam-node-predicate
	nil
	'("person" "project")))))

  (defun ch/org/go-inbox ()
    (interactive)
    (ch/org/go
      (find-file (concat ch/org/directory "inbox.org"))))

  (defun ch/org/refile ()
    (interactive)
    (org-roam-refile))

  (defun ch/org/go-todo-candidates ()
    (mapcar
     (lambda (headline)
       (list (org-element-property :raw-value headline)
	     (org-element-property :org-marker headline)))
     (org-ql-select (ch/org/files)
       '(todo)
       :action 'element-with-markers)))

  (defun ch/org/go-todo ()
    (interactive)
    (ch/org/go
      (ivy-read "Goto: " (ch/org/go-todo-candidates)
		:action (lambda (x) (org-goto-marker-or-bmk (cadr x)))
		:caller 'ch/org/go-todo)))

  (defun ch/org/go-search ()
    (interactive)
    (ch/org/go
      (counsel-rg "" ch/org/directory)))

  (ch/crockeo/register-keys
    ("C-c C-w C-a" . ch/org/go-anytime)
    ("C-c C-w C-c" . ch/org/capture)
    ("C-c C-w C-f" . ch/org/go-find-node)
    ("C-c C-w C-g" . ch/org/go-todo)
    ("C-c C-w C-i" . ch/org/go-inbox)
    ("C-c C-w C-k" . ch/org/go-find-knowledge)
    ("C-c C-w C-l" . ch/org/go-logbook)
    ("C-c C-w C-m" . ch/org/refile)
    ("C-c C-w C-o" . ch/org/go-oneday)
    ("C-c C-w C-p" . ch/org/go-find-project)
    ("C-c C-w C-q" . ch/winconf/pop)
    ("C-c C-W C-s" . ch/org/go-search)
    ("C-c C-w C-t" . ch/org/go-today)
    ("C-c C-w C-u" . ch/org/go-upcoming)
    ("C-c C-w C-x" . org-archive-subtree)
    )
  )
