(require 'failing-tests)
(require 'helm-projectile)

(load-library "buffer-queries")
(load-library "format")
(load-library "winpop")

(defun copy-current-file-path ()
  "Copies the path of the current file relative to its project path into the
clipboard."
  (interactive)
  (let* ((root (projectile-project-root))
         (full-path buffer-file-name)

         (relative-path (string-remove-prefix root full-path)))
    (kill-new relative-path)))

;; Divided up hotkeys into individual sections. Note that certain hotkeys, which
;; are not global across emacs, are stored in individual files according to
;; their (major|minor)-mode.
(defvar crockeo-mode-hotkeys '())
(setq crockeo-mode-hotkeys
      '(("C-c c" evilnc-comment-or-uncomment-lines)

        ("C-c d" dired-sidebar-toggle-sidebar)

        ("C-c b b" switch-to-buffer)
        ("C-c b f" move-to-file-buffer)
        ("C-c b i" move-to-irc-buffer)

        ("C-c p a" helm-projectile-ag)
        ("C-c p c" copy-current-file-path)
        ("C-c p f" helm-projectile-find-file)
        ("C-c p p" helm-projectile-switch-project)
        ("C-c p r" projectile-discover-projects-in-search-path)

        ("C-c g f" failing-tests-from-pr-url)
        ("C-c g l" git-link)

        ("C-c i" ibuffer)

        ("C-c j b" smart-jump-back)
        ("C-c j g" smart-jump-go)

        ("C-c k" kill-buffer-and-window)

        ("C-c s" format-buffer)

        ("C-c w b" winpop-pop)
        ("C-c w c" winpop-clear)
        ("C-c w f" winpop-push)
        ("C-c w h" winpop-go-home)))

(defun register-keys (keymap hotkeys)
  "Registers a series of keys onto a keymap."
  (mapc
   (lambda (keybind)
     (define-key
       keymap
       (kbd (nth 0 keybind))
       (nth 1 keybind)))
   hotkeys)
  keymap)

;; When you want to redefine your keys, after modifying crockeo-mode-hotkeys,
;; run the following:
;;
;; (register-keys crockeo-mode-map crockeo-mode-hotkeys)

;; Defining the crockeo-mode keymap.
(defun make-crockeo-mode-keymap ()
  "Constructs a keymap for crockeo-mode, and then registers the
  crockeo-mode-hotkeys to it."
  (let ((crockeo-mode-map (make-sparse-keymap)))
    (register-keys crockeo-mode-map
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
