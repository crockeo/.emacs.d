;;; magit.el -*- lexical-binding: t; -*-

(ch/pkg magit
  (defun ch/magit/visit-pr ()
    (interactive)
    (browse-url
     (concat
      "https://github.com/"
      (replace-regexp-in-string
       "git@github\\.com:"
       ""
       (magit-get "remote" (magit-get-remote "master") "url"))
      "/pull/new/"
      (magit-get-current-branch))))

  (use-package magit
    :config
    (define-key magit-mode-map "h" #'ch/magit/visit-pr)))
