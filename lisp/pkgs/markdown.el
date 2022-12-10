;;; markdown.el -*- lexical-binding: t; -*-

(ch/pkg markdown
  (use-package markdown-mode)

  (defun ch/markdown/configure ()
    (indent-tabs-mode -1)
    (flyspell-mode-on))

  (add-hook 'markdown-mode-hook #'ch/markdown/configure))
