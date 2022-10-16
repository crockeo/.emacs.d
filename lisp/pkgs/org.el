;;; org.el -*- lexical-binding: t; -*-

(ch/pkg org
  (use-package org
    :config
    (setq org-todo-keywords '((sequence "TODO" "NEXT" "WAITING" "|" "DONE"))))

  (use-package org-contrib
    :after org)

  (use-package org-modern
    :after org
    :hook (org-mode . org-modern-mode)
    :config
    (setq org-modern-label-border 1)
    (setq org-modern-todo-faces
	  '(("TODO"
	     :weight bold
	     :foreground "gray30"
	     :background "gray90")
	    ("NEXT"
	     :weight bold
	     :foreground "white"
	     :background "DodgerBlue1")
	    ("WAITING"
	     :weight bold
	     :foreground "white"
	     :background "gray50")
	    ("DONE"
	     :weight bold
	     :foreground "white"
	     :background "green4"))))

  (use-package org-roam
    :after org
    :config
    (setq org-roam-directory (expand-file-name org-directory)))

  (use-package org-transclusion
    :after org
    :bind ("C-c o t" . org-transclusion-add))

  (defun ch/org/config ()
    (diff-hl-mode -1)
    (display-line-numbers-mode -1)
    (org-indent-mode))

  (add-hook 'org-mode-hook #'ch/org/config)

  (defun ch/org/go-home ()
    (interactive)
    (let ((winconf (current-window-configuration)))
      (condition-case nil
	  (progn
	    (find-file (expand-file-name "~/org/home.org"))
	    (delete-other-windows)
	    (ch/winconf/save winconf))
	(error (ch/winconf/pop winconf)))))

  (defun ch/org/go-node ()
    (interactive)
    (let ((winconf (current-window-configuration)))
      (org-roam-node-find)
      (delete-other-windows)
      (ch/winconf/save winconf))))
