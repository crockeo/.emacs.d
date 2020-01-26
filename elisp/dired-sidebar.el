(require 'dired)

;; Toggling directories in dired sidebar
(define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)
