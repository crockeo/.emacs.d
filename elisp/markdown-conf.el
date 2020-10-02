;; Install packages
(use-package markdown-mode)

;; Setting fill mode
(defun markdown-fill ()
  (set-fill-column 120)
  (auto-fill-mode 1))

(add-hook 'markdown-mode-hook 'markdown-fill)
