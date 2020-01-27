(require 'helm-projectile)

(load-library "format")

;; Defining the crockeo-mode keymap.
;;
;; Divided up hotkeys into individual sections. Note that certain hotkeys, which
;; are not global across emacs, are stored in individual files according to
;; their (major|minor)-mode.
(defun make-crockeo-mode-keymap ()
  (let ((crockeo-mode-map (make-sparse-keymap)))
    ;; evil-mode
    (define-key crockeo-mode-map (kbd "C-c c") 'evilnc-comment-or-uncomment-lines)

    ;; dired-sidebar
    (define-key crockeo-mode-map (kbd "C-c d") 'dired-sidebar-toggle-sidebar)

    ;; helm-projectile
    (define-key crockeo-mode-map (kbd "C-c a") 'helm-projectile-ag)
    (define-key crockeo-mode-map (kbd "C-c f") 'helm-projectile-find-file)
    (define-key crockeo-mode-map (kbd "C-c p") 'helm-projectile-switch-project)

    ;; misc
    (define-key crockeo-mode-map (kbd "C-c e") 'eshell)
    (define-key crockeo-mode-map (kbd "C-c i") 'ibuffer)
    (define-key crockeo-mode-map (kbd "C-c s") 'format-buffer)

    crockeo-mode-map))

;; Defining crockeo-mode.
(define-minor-mode crockeo-mode
  "Personal minor-mode"
  :lighter " crockeo"
  :keymap (make-crockeo-mode-keymap))

;; Globalizing crockeo-mode, so it works across buffers. Namely useful for when
;; I open dired-sidebar.
(define-globalized-minor-mode global-crockeo-mode
  crockeo-mode
  (lambda ()
    (crockeo-mode 1)))

(provide 'crockeo-mode)
(provide 'global-crockeo-mode)
