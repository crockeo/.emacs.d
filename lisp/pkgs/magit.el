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
    (define-key magit-mode-map "h" #'ch/magit/visit-pr)
    (setq magit-clone-default-directory (expand-file-name "~/src/"))

    ;; don't refresh status buffer.
    ;; makes things slightly faster
    (setq magit-refresh-status-buffer nil)

    (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

    ;; don't see diff when comitting.
    (remove-hook 'server-switch-hook 'magit-commit-diff)
    (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))

  (advice-add
   'magit-clone-internal
   :after
   (lambda (&rest _args)
     (projectile-discover-projects-in-search-path))))
