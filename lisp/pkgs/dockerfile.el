;;; dockerfile.el -*- lexical-binding: t; -*-

(ch/pkg dockerfile
  (use-package dockerfile-mode
    :config
    (setq dockerfile-enable-auto-indent nil))

  (defun ch/dockerfile/last-non-whitespace ()
    (save-excursion
      (previous-line)
      (end-of-line)
      (skip-chars-backward "[ \t]" (point-at-bol))
      (string (preceding-char))))

  (defun ch/dockerfile/indent (orig-fn &rest args)
    (let* ((last-non-whitespace (ch/dockerfile/last-non-whitespace))
	   (indent-target (if (string-equal last-non-whitespace "\\")
			      dockerfile-indent-offset
			    0)))
      (beginning-of-line)
      (skip-chars-forward "[ \t]" (point-at-eol))
      (indent-to indent-target)))

  (advice-add 'dockerfile-indent-line-function :around #'ch/dockerfile/indent))
