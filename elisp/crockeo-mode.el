(require 'helm-projectile)

(load-library "buffer-queries")
(load-library "format")
(load-library "winpop")

;; Divided up hotkeys into individual sections. Note that certain hotkeys, which
;; are not global across emacs, are stored in individual files according to
;; their (major|minor)-mode.
(defvar crockeo-mode-hotkeys '())
(setq crockeo-mode-hotkeys
      '(("C-c a" helm-projectile-ag)
        ("C-c c" evilnc-comment-or-uncomment-lines)
        ("C-c d" dired-sidebar-toggle-sidebar)
        ("C-c b b" switch-to-buffer)
        ("C-c b f" move-to-file-buffer)
        ("C-c b i" move-to-irc-buffer)
        ("C-c p a" helm-projectile-ag)
        ("C-c p f" helm-projectile-find-file)
        ("C-c p p" helm-projectile-switch-project)
        ("C-c p r" projectile-discover-projects-in-search-path)
        ("C-c g b" dumb-jump-back)
        ("C-c g f" dumb-jump-go)
        ("C-c i" ibuffer)
        ("C-c k" kill-buffer-and-window)
        ("C-c s" format-buffer)
        ("C-c w b" winpop-pop)
        ("C-c w f" winpop-push)
        ("C-c w h" winpop-go-home)))

;; Defining the crockeo-mode keymap.
(defun make-crockeo-mode-keymap ()
  (let ((crockeo-mode-map (make-sparse-keymap)))
    (mapc
     (lambda (keybind)
       (define-key
         crockeo-mode-map
         (kbd (nth 0 keybind))
         (nth 1 keybind)))
     crockeo-mode-hotkeys)
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
