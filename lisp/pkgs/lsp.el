;; lsp.el -*- lexical-binding: t; -*-

(ch/pkg lsp (prog-mode-hook)
  (use-package lsp-mode
    :init (setq lsp-keymap-prefix "C-c l")
    :config
    ;; only want to add the ignore str once
    ;; so i have to do this cursed nonsense
    (let ((venv-str "[/\\\\]\\venv\\'"))
      (unless (member venv-str lsp-file-watch-ignored-directories)
	(push venv-str lsp-file-watch-ignored-directories)))
    (setq lsp-enable-on-type-formatting nil
	  lsp-enable-symbol-highlighting nil
	  lsp-headerline-breadcrumb-enable nil
	  lsp-idle-delay 0.25))

  (use-package lsp-ui
    :config
    (setq lsp-ui-doc-position 'top
	  lsp-ui-doc-show-with-mouse nil)))
