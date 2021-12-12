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

  (defun ch/org/change-level (headline level-diff)
    (let ((demoted-children (org-ml-headline-map-subheadlines
			      (lambda (subheadlines)
				(mapcar (lambda (el) (ch/org/change-level el level-diff))
					subheadlines))
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

  ;; TODO: integrate this better with existing refile?
  ;; which may also handle the part about like
  ;; demoting or promoting the levels of each thing
  (defun ch/org/custom-refile ()
    (interactive)
    (let* ((headline (org-ml-parse-this-subtree))
	   (begin (org-element-property :begin headline))
	   (end (org-element-property :end headline))

	   (dest (ch/org/select-headline))
	   (dest-end (org-element-property :end dest))

	   (insert-pos (- dest-end
			  (if (< end dest-end)
			      (- end begin)
			    0)))

	   (leveled-headline (ch/org/change-level headline (+ 1 (- (org-element-property :level dest)
								   (org-element-property :level headline))))))
      (kill-region (org-element-property :begin headline)
		   (org-element-property :end headline))
      (org-ml-insert insert-pos leveled-headline)))

  (defun ch/org/archive ()
    (interactive)
    (let* ((headline (org-ml-parse-this-subtree))
	   (demoted-headline (ch/org/change-level headline 1))
	   (path (ch/org/current-olp)))
      (kill-region (org-element-property :begin headline)
		   (org-element-property :end headline))
      (let* ((insert-path (cons "archive" (butlast path)))
	     (insert-point (ch/org/create-path insert-path)))
	(org-ml-insert insert-point demoted-headline))))

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
      (delete-other-windows)
      (find-file "~/home.org")
      (let ((home-window (get-buffer-window))
	    (agenda-window (split-window-horizontally)))
	(select-window agenda-window)
	(org-agenda-list)
	(org-agenda-day-view)
	(select-window home-window)
	(org-overview))))

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

  (defun ch/org/home/go-back ()
    (interactive)
    (if ch/org/home/window-configuration
	(progn
	  (set-window-configuration ch/org/home/window-configuration)
	  (setq ch/org/home/window-configuration nil))
      (message "No prior window configuration.")))

  (defun ch/org/capture-hook ()
    (ch/org/custom-refile))

  (add-hook 'org-capture-before-finalize-hook #'ch/org/capture-hook)
  ;; (remove-hook 'org-capture-after-finalize-hook #'ch/org/capture-hook)

  ;; without this, we leave a stray ch/org/home/window-configuration
  ;; which messes up the next ch/org/home/go-*
  (defadvice org-agenda-quit (around advice-org-agenda-quit activate)
    (interactive)
    (if ch/org/home/window-configuration
	(ch/org/home/go-back)
      (advice-org-agenda-quit)))

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
    ((org-mode . ch/org/config)))

  (use-package org-ml
    :after org)

  (use-package org-ql
    :after org)

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

  (setq org-refile-use-outline-path t)

  (use-package doct
    :config
    (setq org-capture-templates
	  (doct '(("Task"
		   :keys "t"
		   :file "~/home.org"
		   :olp ("captures")
		   :template ("* TODO %^{Description} :%^G:"
			      "SCHEDULED: %^{Scheduled}t"
                              "%?"))

		  ("Backlog"
		   :keys "b"
		   :file "~/home.org"
		   :olp ("captures")
		   :template ("* TODO %^{Description} :%^G:"
			      "%?"))

		  ("Note"
		   :keys "n"
		   :file "~/home.org"
		   :prepend t
		   :olp ("captures")
		   :template ("* %^{Description}"
			      "%?")))))))
