;; Scrolling smoothly, rather than page-by-page
(setq scroll-conservatively 101)

;; Maintaining a 4 line margin
(setq scroll-margin 4)

;; Indent when pressing return
(global-set-key (kbd "RET") 'newline-and-indent)

;; Remove trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Moving all backups to a single directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; Moving all autosaves to a single directory
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/backup/" t)))

;; Removing emacs menu bar / tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Setting tabs to spaces
(setq-default indent-tabs-mode nil)

;; Viewing (row,col) in while editing.
(column-number-mode 1)

;; Turning of Eldoc, because it spawns new buffers
(global-eldoc-mode -1)

;; Turning off bell sound...because it sucks
(setq ring-bell-function 'ignore)

;; Making the initial buffer *scratch*, instead of *GNU Emacs*, so that
;; crockeo-mode is enabled by default.
(setq initial-buffer-choice "~/home.org")

;; Making org-mode have an auto fill at column 80.
(defun org-mode-fill ()
  (set-fill-column 120)
  (auto-fill-mode 1))

(add-hook 'org-mode-hook 'org-mode-fill)

;; Making Emacs GC more kindly
(setq gc-cons-threshold (eval-when-compile (* 2 1024 1024 1024)))
(run-with-idle-timer
 3
 t
 (lambda () (garbage-collect)))

;; Allow for larger read process outputs
(setq read-process-output-max (eval-when-compile (* 1024 1024)))

;; Prefer disk file to Emacs file
(global-auto-revert-mode t)

;; Increase the max recursion size
(setq max-specpdl-size 5000)
