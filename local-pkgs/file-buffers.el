(defun file-buffers ()
  (seq-filter
   #'buffer-file-name
   (buffer-list)))

(defun last-file-buffer ()
  (let ((bufs (file-buffers)))
    (when (> (length bufs) 1)
      (cadr bufs))))

(defun switch-to-last-file-buffer ()
  (interactive)
  (switch-to-buffer (last-file-buffer)))

(provide 'file-buffers)
