;; Installing packages
(use-package helm
  :ensure t
  :init (helm-mode))

(use-package helm-ag
  :after helm
  :ensure t)

(use-package helm-projectile
  :after helm projectile
  :ensure t)

;; Enable fuzzy match whenever helm-mode is enabled.
(defvar helm-mode-fuzzy-match)
(setq helm-mode-fuzzy-match t)
