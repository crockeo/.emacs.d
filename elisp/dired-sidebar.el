(require 'dired)

;; Toggling directories in dired sidebar
(define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)

;; Removing line numbers when we're in dired-sidebar
(defun disable-line-numbers (&rest ignore)
  (message "testing")
  (display-line-numbers-mode -1))

(add-hook 'dired-mode-hook 'disable-line-numbers)
