;; Installing packages
(use-package helm
  :init (helm-mode))

(use-package helm-ag
  :after helm)

(use-package helm-projectile
  :after helm projectile)

;; Enable fuzzy match whenever helm-mode is enabled.
(defvar helm-mode-fuzzy-match)
(setq helm-mode-fuzzy-match t)
