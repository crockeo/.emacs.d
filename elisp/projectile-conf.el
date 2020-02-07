;; Installing packages
(use-package projectile
  :ensure t
  :init (projectile-mode))

(use-package helm-projectile
  :after helm projectile
  :ensure t)

;; Adding projectile projects
(setq
 projectile-project-search-path
 '("~/src/"))

(defun setup-projectile-projects ()
  (projectile-discover-projects-in-search-path)

  (let ((emacs-dir "~/.emacs.d/"))
    (when (not (member emacs-dir projectile-known-projects))
      (projectile-add-known-project emacs-dir))))

(setup-projectile-projects)
