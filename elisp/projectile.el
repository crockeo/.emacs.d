(require 'projectile)

;; Enabling fuzzy search
(defun setup-projectile ()
  (define-key projectile-mode-map (kbd "C-c a") 'helm-projectile-ag)
  (define-key projectile-mode-map (kbd "C-c f") 'helm-projectile-find-file)
  (define-key projectile-mode-map (kbd "C-c p") 'helm-projectile-switch-project))

(add-hook 'projectile-mode-hook 'setup-projectile)
