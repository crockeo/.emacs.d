;; Installing packages
(use-package evil
  :init (evil-mode))

;; Adds multi-line commenting
(use-package evil-nerd-commenter)

(require 'evil)

;; Escaping from INSERT mode more easily
(evil-define-key nil evil-insert-state-map
  "\C-f" 'evil-normal-state)

;; Moving to the beginning / end of a line.
(defun get-line ()
  (buffer-substring
   (line-beginning-position)
   (line-end-position)))

(defun evil-insert-end-of-line ()
  (interactive)
  (evil-end-of-line)
  (if (not (equal (get-line) ""))
      (forward-char)
    nil))

(evil-define-key nil evil-insert-state-map
  "\C-a" 'evil-beginning-of-line
  "\C-e" 'evil-insert-end-of-line)

(evil-define-key nil evil-normal-state-map
  "\C-a" 'evil-beginning-of-line
  "\C-e" 'evil-end-of-line)

(evil-define-key nil evil-visual-state-map
  "\C-a" 'evil-beginning-of-line
  "\C-e" 'evil-end-of-line)

;; Swapping back and forth between active *file* buffers.
(defun get-other-file-buffer ()
  "Returns the 'other' file buffer. Used to swap back and forth between buffers,
  but only considers those which correspond to files."
  (let ((file-buffers (seq-filter
                       (lambda (buffer) (buffer-file-name buffer))
                       (buffer-list))))
    (when (> (length file-buffers) 1)
      (nth 1 file-buffers))))

(defun switch-to-other-file-buffer ()
  "Switching to the last file buffer."
  (interactive)
  (switch-to-buffer (get-other-file-buffer)))

(evil-define-key nil evil-normal-state-map
  ";" 'switch-to-other-file-buffer)
