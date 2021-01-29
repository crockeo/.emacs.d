(defvar winpop-confs '())

(defun winpop-push ()
  (interactive)
  (push (current-window-configuration) winpop-confs))

(defun winpop-pop ()
  (interactive)
  (let ((old-winpop-conf (pop winpop-confs)))
    (when old-winpop-conf
      (set-window-configuration old-winpop-conf))))

(defun winpop-clear ()
  (interactive)
  (setq winpop-confs '()))

(defun winpop-go-home ()
  (interactive)
  (winpop-push)
  (delete-other-windows)
  (let ((home-buffer (get-file-buffer initial-buffer-choice)))
    (if home-buffer
        (switch-to-buffer home-buffer)
      (find-file initial-buffer-choice))))

(provide 'winpop)
