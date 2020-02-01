;; Installing the package
(use-package git-gutter-fringe
  :ensure t)

(require 'fringe-helper)

;; git-gutter-mode stays on, but does not render diffs, while changing buffers.
;; This automatically recalculates git information in the gutter when changing
;; buffers.
(defvar-local last-current-buffer nil)

(defun git-gutter-re-enable ()
  (when (not (equal (current-buffer) last-current-buffer))
    (progn
      (setq last-current-buffer (current-buffer))
      (git-gutter))))

(add-hook 'buffer-list-update-hook 'git-gutter-re-enable)

;; Adding a margin to the frigne for git-gutter-mode
(defun add-fringe-margin ()
  (fringe-mode 16))

(add-hook 'git-gutter-mode-on-hook 'add-fringe-margin)

;; Changing the icons to be more to my liking
(fringe-helper-define 'git-gutter-fr:modified nil
  "........"
  "........"
  "........"
  ".###...#"
  "##.##.##"
  "#...###."
  "........"
  "........")

;; Changing the default colors
(set-face-foreground 'git-gutter-fr:added "green")
(set-face-foreground 'git-gutter-fr:modified "yellow")
(set-face-foreground 'git-gutter-fr:deleted "red")

;; Enabling git-gutter-mode by default.
(global-git-gutter-mode 1)
(add-fringe-margin)
