;; Installing packages
(use-package projectile
  :ensure t
  :init (projectile-mode))

(use-package helm-projectile
  :after helm projectile
  :ensure t)
