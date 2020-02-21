(defvar winpop-confs '())
(defvar winpop-names '())

(defun winpop-get-confs ()
  "Returns the current list of configurations."
  winpop-confs)

(defun winpop-get-names ()
  "Returns the names of each of the configurations."
  winpop-names)

(defun winpop-push ()
  "Stores the current window configuration on the stack of window
  configurations. Meant to be used before modifying the window configuration."
  (interactive)
  (push (current-window-configuration) winpop-confs)
  (push (buffer-name) winpop-names)
  (message "winpop: Pushed window configuration %s" (buffer-name)))

(defun winpop-pop ()
  "Takes the top window configuration from the top of the stack and sets it as
  the current window configuration."
  (interactive)
  (let ((old-winpop-conf (pop winpop-confs))
        (old-winpop-name (pop winpop-names)))
    (if old-winpop-conf
        (progn
          (set-window-configuration old-winpop-conf)
          (message "winpop: Popped window configuration to %s" old-winpop-name))
      (message "winpop: At bottom of stack"))))

(defun winpop-clear ()
  "Clears the stack of window configurations, but does not go back to any
  particular one."
  (interactive)
  (setq winpop-confs '())
  (setq winpop-names '()))

(defun winpop-go-home ()
  "Moves the current window to the file described by initial-buffer-choice. At
  time of writing, that's ~/home.org"
  (interactive)
  (winpop-push)
  (delete-other-windows)

  ;; Go to ~/home.org
  (let ((existing-buffer (get-file-buffer initial-buffer-choice)))
    (if existing-buffer
        (switch-to-buffer existing-buffer)
      (find-file initial-buffer-choice)))

  ;; Opening weekly agenda
  (org-agenda nil "a")

  ;; Switching to the home.org buffer
  (other-window 1))

(provide 'winpop-get-confs)
(provide 'winpop-get-names)
(provide 'winpop-push)
(provide 'winpop-pop)
(provide 'winpop-clear)
(provide 'winpop-go-home)
