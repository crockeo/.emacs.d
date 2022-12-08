;;; python.el -*- lexical-binding: t; -*-

(ch/pkg python
  (use-package lsp-pyright
    :init (setq lsp-pyright-multi-root nil)
    :hook (python-mode . (lambda ()
			   (require 'lsp-pyright)
			   (let ((venv-dir (concat (projectile-project-root) "venv")))
			     (when (file-directory-p venv-dir)
			       (pyvenv-activate venv-dir)))
			   (lsp-deferred)))
    :config
    (setq lsp-pyright-log-level "error"
	  lsp-pyright-multi-root nil))

  (use-package pyvenv)

  (defun ch/python/indentation-override (orig-fun &rest args)
    (save-excursion
      (pcase (python-indent-context)
	;; when inside of a function definition, e.g.
	;;
	;; def func(
	;;     argument: str
	;; ): -> str:
	;;     pass
	;;
	;; do not try to indent argument two indentation widths
	;; instead just prefer one!
	(`(:inside-paren-newline-start-from-block . ,start)
	 (goto-char start)
	 (+ (current-indentation) python-indent-offset))

	(_ (apply orig-fun args)))))

  (advice-add 'python-indent--calculate-indentation :around #'ch/python/indentation-override))
