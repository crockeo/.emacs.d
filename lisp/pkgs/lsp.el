;;; lsp.el -*- lexical-binding: t; -*-

(ch/pkg lsp
  (use-package lsp-mode
    :init (setq lsp-keymap-prefix "C-c l")
    :config
    ;; only want to add the ignore str once
    ;; so i have to do this cursed nonsense
    (let ((venv-str "[/\\\\]\\venv\\'"))
      (unless (member venv-str lsp-file-watch-ignored-directories)
	(push venv-str lsp-file-watch-ignored-directories)))

    (setq lsp-enable-on-type-formatting nil
	  lsp-enable-snippet nil
	  lsp-enable-symbol-highlighting nil
	  lsp-headerline-breadcrumb-enable nil
	  lsp-idle-delay 0.25
	  lsp-modeline-diagnostics-scope :file)

    ;; prevents LSP from opening all projects
    ;; associated with the language server
    (advice-add
     'lsp
     :before (lambda (&rest _args)
	       (eval '(setf (lsp-session-server-id->folders (lsp-session))
			    (ht))))))

  (use-package lsp-ui
    :after (lsp)
    :config
    (setq lsp-ui-doc-position 'top
	  lsp-ui-doc-show-with-mouse nil)))
