;; Installing packages
(use-package dired-sidebar
  :config
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

(use-package all-the-icons-dired)

(require 'dired)

;; Toggling directories in dired sidebar
(define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle)

;; Removing line numbers when we're in dired-sidebar
(defun disable-line-numbers (&rest ignore)
  (display-line-numbers-mode -1))

(add-hook 'dired-mode-hook 'disable-line-numbers)
