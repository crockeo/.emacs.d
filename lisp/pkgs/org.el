;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (defvar ch/org/directory (file-name-as-directory (expand-file-name "~/org")))
  (setq org-directory ch/org/directory)

  (use-package org
    :custom
    (org-agenda-hide-tags-regexp ".")
    (org-agenda-tags-column 0)
    (org-auto-align-tags nil)
    (org-capture-bookmark nil)
    (org-default-notes-file (concat ch/org/directory "inbox.org"))
    (org-element-use-cache nil)
    (org-hide-emphasis-markers t)
    (org-link-descriptive t)
    (org-log-done 'time)
    (org-startup-folded t)
    (org-startup-truncated nil)
    (org-tags-column 0)
    (org-tags-exclude-from-inheritance '("project" "area" "reference"))
    (org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "DONE")))

    :custom-face
    (org-code ((t (:background ,(modus-themes-get-color-value 'bg-inactive)))))
    (org-level-1 ((t (:height 1.15))))
    (org-level-2 ((t (:height 1.10))))
    (org-level-3 ((t (:heihgt 1.05))))

    :config
    (evil-define-key 'normal org-mode-map
      (kbd "<tab>") #'org-cycle))

  (use-package org-appear
    :after org
    :hook (org-mode . org-appear-mode)
    :config
    (setq
     org-appear-trigger 'manual
     org-appear-autoemphasis t
     org-appear-autolinks t
     )
    (add-hook
     'org-mode-hook
     (lambda ()
       (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
       (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))))

  (use-package org-autolist
    :after org
    :hook (org-mode . org-autolist-mode))

  (use-package org-contrib
    :after org)

  (use-package org-modern
    :after org
    :hook ((org-mode . org-modern-mode)
	   (org-agenda-finalize . org-modern-agenda))
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
    (org-indent-mode)
    (setq olivetti-minimum-body-width 80)

    ;; Make code look like code
    (set-face-attribute 'org-code
			nil
			:background (modus-themes-get-color-value 'bg-inactive))

    )

  (defun ch/org/config-agenda ()
    (diff-hl-mode -1)
    (display-line-numbers-mode -1)
    (setq olivetti-minimum-body-width 80))

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

  ;; Step 0) Separate work from life
  (defvar ch/org/mode "home")

  (defun ch/org/mode-other ()
    (pcase ch/org/mode
      ("home" "work")
      ("work" "home")))

  (defun ch/org/swap-mode ()
    (interactive)
    (let ((new-mode
	   (setq ch/org/mode (ch/org/mode-other))))
      (message "Now in mode `%s`." new-mode)))

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
  (defun ch/org/go-today ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	`(and (or (ts :on today)
		  (ts :before today))
	      (todo)
	      (not (tags ,(ch/org/mode-other))))
	:title "Today"
	:super-groups '((:auto-map (lambda (item) (ch/org/category)))))))

  (defun ch/org/go-anytime ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	`(and (todo)
	      (not (ts-active))
	      (not (tags "oneday"))
	      (not (tags ,(ch/org/mode-other))))
	:title "Anytime"
	:super-groups '((:auto-map (lambda (item) (ch/org/category)))))))

  (defun ch/org/go-oneday ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	`(and (todo)
	      (not (ts-active))
	      (tags "oneday")
	      (not (tags ,(ch/org/mode-other))))
	:title "Oneday"
	:super-groups '((:auto-map (lambda (item) (ch/org/category)))))))

  (defun ch/org/go-logbook ()
    (interactive)
    (ch/org/go
      (org-ql-search
	(ch/org/files)
	`(and (done)
	      (not (tags ,(ch/org/mode-other))))
	:title "Inbox"
	:sort 'date ;; TODO: descending?
	)
      ))

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
	'("project")
	`(,(ch/org/mode-other)
	  ,@ch/org/go-blocklist)))))

  (defun ch/org/go-find-knowledge ()
    (interactive)
    (ch/org/go
      (org-roam-node-find
       nil
       ""
       (ch/org/roam-node-predicate
	nil
	(list "project" (ch/org/mode-other))
	))))

  ;; Step 1) Separate kinds of things I'm looking for
  (defun ch/org/find-current-mode ()
    (interactive)
    (org-roam-node-find
     nil
     ""
     (ch/org/roam-node-predicate
      '("project")
      `(
	,(ch/org/mode-other)
	,@ch/org/go-blocklist))))

  (defun ch/org/find-knowledge ()
    )

  ;; TODO: decide if i need this
  ;; (defun ch/org/go-find-area ()
  ;;   (interactive)
  ;;   (ch/org/go
  ;;     (org-roam-node-find nil nil
  ;; 			  (ch/org/roam-node-predicate '("area") nil))))

  ;; (defun ch/org/go-find-project ()
  ;;   (interactive)
  ;;   (ch/org/go
  ;;     (org-roam-node-find nil nil
  ;; 			  (ch/org/roam-node-predicate '("project") nil))))

  (defun ch/org/go-inbox ()
    (interactive)
    (ch/org/go
      (find-file (concat ch/org/directory "inbox.org"))))

  (defun ch/org/refile ()
    (interactive)
    (org-roam-refile))

  (ch/crockeo/register-keys
    ;; Work-life balance :)
    ("C-c C-w C-w" . ch/org/swap-mode)

    ;; Temporal
    ("C-c C-w C-t" . ch/org/go-today)
    ("C-c C-w C-a" . ch/org/go-anytime)
    ("C-c C-w C-o" . ch/org/go-oneday)
    ("C-c C-w C-l" . ch/org/go-logbook)

    ;; Spatial
    ("C-c C-w C-c" . ch/org/capture)
    ("C-c C-w C-f" . ch/org/go-find-node)
    ("C-c C-w C-k" . ch/org/go-find-knowledge)
    ("C-c C-w C-i" . ch/org/go-inbox)
    ("C-c C-w C-m" . ch/org/refile)

    ;; TODO: decide if i need this
    ;; ("C-c C-w C-a" . ch/org/go-find-area)
    ;; ("C-c C-w C-p" . ch/org/go-find-project)

    ;; Misc
    ("C-c C-w C-q" . ch/winconf/pop)
    )
  )
