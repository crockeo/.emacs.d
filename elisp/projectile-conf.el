;; Installing packages
(use-package projectile
  :init (projectile-mode))

(use-package helm-projectile
  :after helm projectile)

;; Adding projectile projects
(setq
 projectile-project-search-path
 '("~/src/"
   "~/src/go/src/github.com/crockeo"))

(defun setup-projectile-projects ()
  (projectile-discover-projects-in-search-path)

  (let ((emacs-dir "~/.emacs.d/"))
    (when (not (member emacs-dir projectile-known-projects))
      (projectile-add-known-project emacs-dir))))

(setup-projectile-projects)
