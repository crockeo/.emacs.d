;;; magit.el -*- lexical-binding: t; -*-

(ch/pkg magit
  (defun ch/magit/visit-pr/get-repo-name (remote)
    (replace-regexp-in-string
     "\\(git@github\\.com:\\|\\.git$\\)"
     ""
     remote))

  (defun ch/magit/visit-pr ()
    (interactive)
    (browse-url
     (concat "https://github.com/"
	     (ch/magit/visit-pr/get-repo-name (magit-get "remote" "origin" "url"))
	     "/pull/new/"
	     (magit-get-current-branch))))

  (use-package magit
    :config
    (define-key magit-mode-map "h" #'ch/magit/visit-pr)))
