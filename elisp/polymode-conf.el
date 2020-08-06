(defun configure-chicken-scheme-polymode ()
  (define-hostmode poly-scheme-hostmode
    :mode 'scheme-mode)

  (define-innermode poly-scheme-c-innermode
    :mode 'c-mode
    :head-matcher "#>\n"
    :tail-matcher "<#\n"
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode poly-scheme-mode
    :hostmode 'poly-scheme-hostmode
    :innermodes '(poly-scheme-c-innermode)))

(use-package polymode
  :config
  (configure-chicken-scheme-polymode)
  (add-to-list 'auto-mode-alist
               '("\\.ss" . poly-scheme-mode)))
