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

  (defvar ch/org/home-file
    (concat (file-name-as-directory ch/org/org-directory)
	    "home.org"))

  (defun ch/org/update-all-agendas ()
    (interactive)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (derived-mode-p 'org-agenda-mode)
	  (org-agenda-redo t)))))

  (defun ch/org/change-level (level-diff headline)
    (let ((demoted-children (org-ml-headline-map-subheadlines
			      (-partial #'-map (-partial #'ch/org/change-level level-diff))
			      headline)))
      (org-ml-set-property :level (+ level-diff (org-ml-get-property :level demoted-children))
			   demoted-children)))

  (defun ch/org/create-path/get-element (path-part tree)
    (pcase tree
      ('nil nil)
      (`(,head . ,tail)
       (if (string-equal path-part (org-element-property :raw-value head))
	   head
	 (ch/org/create-path/get-element path-part tail)))))

  (defun ch/org/create-path/impl (path tree insert-point insert-level)
    (pcase path
      ('nil insert-point)
      (`(,head . ,tail)
       (let ((element (or (ch/org/create-path/get-element head tree)
			  (org-ml-insert (or insert-point (point-max))
					 (org-ml-build-headline! :level insert-level
								 :title-text head)))))
	 (ch/org/create-path/impl tail
				  (org-ml-get-children element)
				  (or (org-element-property :end element)
				      (+ 2 insert-point (seq-length head) insert-level))
				  (+ 1 insert-level))))))

  (defun ch/org/create-path (path)
    (ch/org/create-path/impl path (org-ml-parse-subtrees 'all) nil 1))

  (defvar ch/org/tree-paths/ignored-nodes '("archive" "backlog" "notes" "tasks"))

  (defun ch/org/tree-paths/impl (tree path)
    (pcase tree
      ('nil nil)
      (`(,head . ,tail)
       (let ((value (org-element-property :raw-value head))
	     (todo-keyword (org-element-property :todo-keyword head)))
	 (when (and value
		    (not todo-keyword)
		    (not (member value ch/org/tree-paths/ignored-nodes)))
	   (cons `(,(string-join (reverse (cons value path)) "/") . ,head)
		(append (ch/org/tree-paths/impl (org-ml-get-children head) (cons value path))
			(ch/org/tree-paths/impl tail path))))))))

  (defun ch/org/tree-paths ()
    (ch/org/tree-paths/impl (org-ml-parse-subtrees 'all) nil))

  (defun ch/org/select-headline ()
    (let* ((tree-paths (ch/org/tree-paths))
	   (chosen-path (ivy-read "Choose headline: "
				  tree-paths
				  :require-match t)))
      (cdr (assoc chosen-path tree-paths))))

  (defun ch/org/refile/core (subtree insert-point &optional level-diff)
    (let* ((begin (org-element-property :begin subtree))
	   (end (org-element-property :end subtree))
	   (insert-point (- insert-point
			    (if (< end insert-point)
				(- end begin)
			      0)))
	   (level-diff (or level-diff 0))
	   (leveled-subtree (ch/org/change-level level-diff subtree)))
      (kill-region begin end)
      (org-ml-insert insert-point leveled-subtree)))

  (defun ch/org/refile/headline (dest-headline)
    (let* ((subtree (org-ml-parse-this-subtree))
	   (level-diff (+ 1 (- (org-element-property :level dest-headline)
			       (org-element-property :level subtree)))))
      (ch/org/refile/core subtree
			  (org-element-property :end dest-headline)
			  level-diff)))

  (defun ch/org/refile/olp (dest-olp)
    (let* ((subtree (org-ml-parse-this-subtree))
	   (insert-point (ch/org/create-path dest-olp))
	   (level-diff (- (+ 1 (length dest-olp))
			  (org-element-property :level subtree))))
      (ch/org/refile/core subtree
			  insert-point
			  level-diff)))

  (defun ch/org/refile ()
    (interactive)
    (let ((dest-headline (ch/org/select-headline)))
      (ch/org/refile/headline dest-headline)))

  (defun ch/org/archive ()
    (interactive)
    (let ((path (ch/org/current-olp)))
      (ch/org/refile/olp (cons "archive" (butlast path)))))

  (defun ch/org/complete ()
    (interactive)
    (org-ml-update (lambda (headline)
		     (->> headline
		       (org-ml-set-property :todo-keyword "DONE")
		       (org-ml-headline-set-planning (org-ml-build-planning! :closed (org-ml-unixtime-to-time-long (current-time))))))
		   (org-ml-parse-this-headline))
    (ch/org/archive))

  (defun ch/org/outline ()
    (org-element-map (org-element-parse-buffer 'headline) 'headline #'identity))

  (defun ch/org/outline-tree/impl/partition-level (outline level)
    ;; splits an outline (only headlines of org doc)
    ;; into two lists:
    ;;
    ;;   - contiguous elements which are at a greater level than the provided level
    ;;   - everything after them
    ;;
    ;; so that other functions can operate on "children" of an outline
    (let* ((head (car outline))
	   (head-level (org-element-property :level head))
	   (tail (cdr outline)))
      (cond
       ((null outline) `(,nil ,nil))
       ((<= head-level level) `(,nil ,outline))
       (t
	(let ((next (ch/org/outline-tree/impl/partition-level tail level)))
	  `(,(cons head (car next)) ,(cadr next)))))))

  (defun ch/org/outline-tree/impl (outline)
    ;; converts a flat org outline into a hierarchy
    ;; such that each headline contains each of the headlines beneath it
    ;; e.g.
    ;;
    ;; * container
    ;;   * sub-container
    ;;   * sub-container peer
    ;; * container peer
    ;;
    ;; becomes
    ;;
    ;; ((container (sub-container) (sub-container peer))
    ;;  (container peer))
    (let* ((container (car outline))
	   (level (org-element-property :level container)))
      (when container
	(pcase-let* ((`(,children ,remaining) (ch/org/outline-tree/impl/partition-level (cdr outline) level))
		     (sub-trees (ch/org/outline-tree/impl children))
		     (peer-trees (ch/org/outline-tree/impl remaining)))
	  `((,container . ,sub-trees)
	    ,@peer-trees)))))

  (defun ch/org/outline-tree ()
    (ch/org/outline-tree/impl (ch/org/outline)))

  (defun ch/org/current-olp/impl (point outline-tree olp)
    ;; given a point and an outline, as constructed by ch/org/outline-tree,
    ;; provide the outline path that corresponds to the current location
    (pcase outline-tree
      ('nil (reverse olp))
      (`(,head . ,tail) (let ((head-element (car head))
			      (head-children (cdr head)))
			  (if (and (>= point (org-element-property :begin head-element))
				   (<= point (org-element-property :end head-element)))
			      (ch/org/current-olp/impl point head-children (cons (org-element-property :raw-value head-element) olp))
			    (ch/org/current-olp/impl point tail olp))))))

  (defun ch/org/current-olp ()
    (let ((outline-tree (ch/org/outline-tree)))
      (ch/org/current-olp/impl (point) outline-tree '())))

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
      (find-file ch/org/home-file)))

  (defun ch/org/home/go-week ()
    (interactive)
    (ch/org/home/toggle
      (org-agenda-list)))

  (defun ch/org/home/go-day ()
    (interactive)
    (ch/org/home/toggle
      (org-agenda-list)
      (org-agenda-day-view)))

  (defun ch/org/home/go-recent ()
    (interactive)
    (ch/org/home/toggle
      (org-ql-view-recent-items :num-days 7
				:type 'closed)))

  (defun ch/org/home/go-yesterday ()
    (interactive)
    (ch/org/home/toggle
      (org-ql-view-recent-items :num-days 1
				:type 'closed)))

  (defun ch/org/home/go-todo ()
    (interactive)
    (ch/org/home/toggle
      (org-todo-list)))

  (defun ch/org/home/go-roam-find ()
    (interactive)
    (ch/org/home/toggle
      (condition-case ()
	  (org-roam-node-find)
	(quit (ch/org/home/go-back)))))

  (defun ch/org/home/go-back ()
    (interactive)
    (if ch/org/home/window-configuration
	(progn
	  (set-window-configuration ch/org/home/window-configuration)
	  (setq ch/org/home/window-configuration nil))
      (message "No prior window configuration.")))

  (defmacro ch/org/agenda/with-headline (&rest body)
    (declare (indent defun))
    `(let* ((marker (org-get-at-bol 'org-marker))
	    (buffer (marker-buffer marker))
	    (pos (marker-position marker)))
       (with-current-buffer buffer
	 (goto-char pos)
	 ,@body)))

  (defun ch/org/agenda/complete ()
    (interactive)
    (ch/org/agenda/with-headline
      (ch/org/complete)
      (ch/org/update-all-agendas)))

  (defun ch/org/agenda/goto-indirect ()
    (interactive)
    (ch/org/agenda/with-headline
      (org-tree-to-indirect-buffer)
      (other-window 1)))

  (defun ch/org/capture-hook ()
    (let* ((headline (org-ml-parse-this-headline))
	   (tags (when headline (org-ml-get-property :tags headline)))
	   (dest-olp (when tags `("project" ,(car tags)))))
      (when dest-olp
	(ch/org/refile-olp dest-olp))))

  (add-hook 'org-capture-before-finalize-hook #'ch/org/capture-hook)
  ;; (remove-hook 'org-capture-after-finalize-hook #'ch/org/capture-hook)

  ;; without this, we leave a stray ch/org/home/window-configuration
  ;; which messes up the next ch/org/home/go-*
  (defadvice org-agenda-quit (around advice-org-agenda-quit activate)
    (interactive)
    (if ch/org/home/window-configuration
	(ch/org/home/go-back)
      (advice-org-agenda-quit)))

  (defun ch/org/config ()
    (setq org-agenda-window-setup 'current-window
	  org-adapt-indentation nil
          org-hide-emphasis-markers t)


    (setq org-agenda-sorting-strategy
	  '((agenda category-keep todo-state-up habit-down time-up priority-down)
	    (todo todo-state-up priority-down category-keep)
	    (tags todo-state-up priority-down category-keep)
	    (search todo-state-up category-keep)))

    (setq org-agenda-files `(,ch/org/home-file))

    (auto-fill-mode 0)
    (display-line-numbers-mode 0)
    (org-indent-mode 1)
    (visual-line-mode 1)

    (add-hook 'after-save-hook
	      #'ch/org/update-all-agendas
	      nil
	      'local))

  (defun ch/org/config-agenda ()
    (dolist (key '("<tab>" "TAB"))
      (define-key org-agenda-keymap (kbd key) #'ch/org/agenda/goto-indirect))

    (define-key org-agenda-keymap (kbd "C-c o c") #'ch/org/agenda/complete))

  (use-package org
    :config
    (setq org-agenda-skip-scheduled-if-done t
	  org-capture-bookmark nil
	  org-directory ch/org/org-directory
	  org-todo-keywords '((sequence "TODO" "NEEDS-REVIEW" "WAITING" "|" "DONE"))
	  org-log-done 'time)
    (progn
      (require 'hawaii-theme)
      (setq org-todo-keyword-faces
	    `(("TODO" . org-warning)
	      ("NEEDS-REVIEW" . ,hawaii-highlight-blue)
	      ("WAITING" . ,hawaii-comment)
	      ("DONE" . ,hawaii-highlight-green))))

    :hook
    ((org-mode . ch/org/config)
     (org-agenda-mode . ch/org/config-agenda)))

  (use-package org-ml
    :after org)

  (use-package org-ql
    :after org)

  (use-package org-roam
    :after org
    :config
    (setq org-roam-directory ch/org/org-directory)
    (org-roam-db-autosync-mode))

  (use-package doct
    :after org
    :config
    (setq org-capture-templates
	  (doct '(("Task"
		   :keys "t"
		   :file ch/org/home-file
		   :template ("* TODO %^{Description} :%^G:"
			      "SCHEDULED: %^{Scheduled}t"
                              "%?"))

		  ("Backlog"
		   :keys "b"
		   :file ch/org/home-file
		   :template ("* TODO %^{Description} :%^G:"
			      "%?"))

		  ("Note"
		   :keys "n"
		   :file ch/org/home-file
		   :template ("* %^{Description} :%^G:"
			      "%?")))))))
