;;; lsp.el -*- lexical-binding: t; -*-

(ch/pkg lsp
  ;; Setting to make LSP faster.
  ;; Set before anything else to make sure that it gets included.
  (setq lsp-pyright-multi-root nil)

  (use-package lsp-mode
    :init
    (setq lsp-use-plists t)

    :config
    (setq lsp-completion-provider :none
          lsp-enable-on-type-formatting nil
          lsp-enable-snippet nil
          lsp-enable-symbol-highlighting nil
          lsp-headerline-breadcrumb-enable nil
          lsp-idle-delay 0.25
          lsp-modeline-diagnostics-scope :file)

    ;; Disable semgrep-ls, because it makes everything run slowwwwwly.
    (setq lsp-semgrep-languages nil))

  (use-package lsp-ui
    :config
    (setq lsp-ui-doc-position 'top
          lsp-ui-doc-show-with-cursor t
	  lsp-ui-doc-show-with-mouse nil))

  (use-package lsp-pyright)

  (with-eval-after-load 'lsp-mode
    ;; Ignore `venv` folders for watching files.
    (let ((venv-str "[/\\\\]\\venv\\'"))
      (unless (member venv-str lsp-file-watch-ignored-directories)
	(push venv-str lsp-file-watch-ignored-directories)))

    ;; ;; Prevent lsp-mode from opening all folders
    ;; (advice-add
    ;;  'lsp
    ;;  :before
    ;;  (lambda (&rest _args)
    ;;    (setf (lsp-session-server-id->folders (lsp-session))
    ;;          (ht))))
    ))
