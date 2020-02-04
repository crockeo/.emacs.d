;; Adds highlighting to appearances of TODO
(defun add-todo-highlight ()
  (font-lock-add-keywords
   nil
   '(("\\<\\(TODO\\)" 1 'font-lock-warning-face prepend))))

;; When enabled, highlight-todo-mode highlights occurrences of "TODO" in the
;; text.
(define-minor-mode highlight-todo-mode
  "Highlights TODOs when they appear in text."
  :lighter ""
  (add-todo-highlight))

;; Globalized verison of highlight-todo-mode so that it happens everywhere.
(define-global-minor-mode global-highlight-todo-mode
  highlight-todo-mode
  (lambda ()
    (highlight-todo-mode 1)))

(provide 'highlight-todo-mode)
(provide 'global-highlight-todo-mode)
