(defun get-buffers-with (predicate)
  "Retrieves all buffers that satisfy some predicate."
  (seq-filter
   predicate
   (buffer-list)))

(defun jump-to-buffer-with (predicate &optional name)
  "Allows the user to interactively select a buffer considering only the subset
  of open buffers that satisfy the provided predicate."
  (interactive)

  (defun advice-predicate (buffers)
    (let ((filtered (seq-filter
                     predicate
                     buffers)))
      filtered))

  (advice-add 'helm-skip-boring-buffers
              :filter-return 'advice-predicate)

  (helm-buffers-list)

  (advice-remove 'helm-skip-boring-buffers
                 'advice-predicate))

(defun move-to-file-buffer ()
  "Select a buffer that corresponds to a file on disk."
  (interactive)
  (jump-to-buffer-with
   (lambda (buffer-name)
     (buffer-file-name (get-buffer buffer-name)))))

(defun move-to-irc-buffer ()
  "Select a buffer that corresponds to an IRC channel."
  (interactive)
  (jump-to-buffer-with
   (lambda (buffer)
     (with-current-buffer buffer
       (eq major-mode 'erc-mode)))))

(setq helm-buffer-max-length nil)

(provide 'move-to-file-buffer)
(provide 'move-to-irc-buffer)
