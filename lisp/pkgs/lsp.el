;;; lsp.el -*- lexical-binding: t; -*-

(ch/pkg lsp
  (use-package lsp-mode)
  (use-package lsp-ui
    :config
    (setq lsp-ui-doc-show-with-cursor t))
  (use-package lsp-ivy)
  (use-package lsp-treemacs)
  (use-package lsp-pyright)

  (with-eval-after-load 'lsp-mode
    ;; Ignore `venv` folders for watching files.
    (let ((venv-str "[/\\\\]\\venv\\'"))
      (unless (member venv-str lsp-file-watch-ignored-directories)
	(push venv-str lsp-file-watch-ignored-directories)))

    ;; Prevent lsp-mode from opening all folders
    ;; (advice-add
    ;;  'lsp
    ;;  :before
    ;;  (lambda (&rest _args)
    ;;    (setf (lsp-session-server-id->folders (lsp-session))
    ;;          (ht))))
    ))
