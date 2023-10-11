;;; python.el -*- lexical-binding: t; -*-

(ch/pkg python
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

  (advice-add 'python-indent--calculate-indentation :around #'ch/python/indentation-override)

  (defun ch/python/start-lsp ()
    (interactive)

    (let ((venv-dir (concat (projectile-project-root) "venv")))
      (when (file-directory-p venv-dir)
	(pyvenv-activate venv-dir))
      (lsp-deferred)))

  (add-hook 'python-mode-hook #'ch/python/start-lsp))
