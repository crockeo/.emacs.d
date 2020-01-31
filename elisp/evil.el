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

;; Jumping to the last buffer that I was working on. Mirroring my vim config.
(defun go-last-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(evil-define-key nil evil-normal-state-map
  ";" 'go-last-buffer)
