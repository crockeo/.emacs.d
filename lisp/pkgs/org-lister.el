;;; org-lister.el -*- lexical-binding: t; -*-

(cl-defun org-lister-return (&optional ignore)
  (interactive "P")
  (when (or ignore (not (org-in-item-p)))
    (cl-return-from org-lister-return (org-return)))

  (let* ((line (org-lister--line-contents))
         (line-without-dash (s-trim-left (s-chop-prefix "-" line)))
         (line-without-checkbox (s-trim-left (s-chop-prefixes '("[ ]" "[X]") line-without-dash))))
    (cond
     ;; We have no list prefix in the current line,
     ;; so pretend that we're not actually in a list anymore.
     ((not (s-starts-with? "-" line))
      (org-return))

     ;; We are inside of a list item, but it has no contents.
     ;; So we are going to delete the list prefix.
     ((s-blank-str? line-without-checkbox)
      (beginning-of-line)
      (delete-region (line-beginning-position) (line-end-position)))

     ;; We are at the beginning of a TODO list item,
     ;; so we should create another one of those.
     ((or (s-starts-with? "[ ]" line-without-dash)
          (s-starts-with? "[X]" line-without-dash))
      (org-insert-todo-heading t))

     ;; We are inside of a list item with normal contents.
     ;; Just create a new list item.
     (t (org-meta-return)))))

(cl-defun org-lister-tab (&optional ignore)
  (interactive "P")
  (when (or ignore (not (org-in-item-p)))
    (cl-return-from org-lister-tab (org-cycle)))

  (save-excursion
    (beginning-of-line)
    (insert "  ")))

(cl-defun org-lister-shift-tab (&optional ignore)
  (interactive "P")
  (when (or ignore (not (org-in-item-p)))
    (cl-return-from org-lister-shift-tab (org-shifttab)))

  (let* ((start (line-beginning-position))
         (end (+ start 2)))
    (when (s-equals? "  " (buffer-substring-no-properties start end))
      (delete-region start end))))

(defun org-lister--line-contents ()
  (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(provide 'org-lister)
