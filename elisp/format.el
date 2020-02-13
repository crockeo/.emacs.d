(require 'blacken)

(defun indent-all ()
  "Performs automatic indentation across an entire buffer. Used for certain file
  types where matching indentation is sufficient."
  (interactive)

  (let ((cursor-pos (point)))
    (mark-whole-buffer)
    (evil-indent (region-beginning)
                 (region-end))
    (goto-char cursor-pos)))

;; The global definition of formatters.
(defvar format-formatters)
(setq
 format-formatters
 '((emacs-lisp-mode . indent-all)
   (funnel-mode . indent-all)
   (go-mode . gofmt)
   (python-mode . blacken-buffer)))

;; Formats the current buffer. Uses a different formatter depending on the
;; current major mode. If an unknown major mode is used, no formatting occurs.
(defun format-buffer ()
  (interactive)
  (let ((cmd (assoc major-mode format-formatters)))
    (if cmd
        (progn
          (funcall (cdr cmd))
          (save-buffer))
      (message "No formatter found for %s" major-mode))))

(provide 'format-buffer)
