(require 'blacken)

;; The global definition of formatters.
(defvar format-formatters)
(setq
 format-formatters
 '((python-mode . blacken-buffer)))

;; Formats the current buffer. Uses a different formatter depending on the
;; current major mode. If an unknown major mode is used, no formatting occurs.
(defun format-buffer ()
  (interactive)
  (let ((cmd (assoc major-mode format-formatters)))
    (if cmd
        (funcall (cdr cmd))
          (message "No formatter found for %s" major-mode))))

(provide 'format-buffer)
