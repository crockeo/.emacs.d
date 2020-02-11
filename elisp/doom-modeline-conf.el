;; Installing the package
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;; Setting up doom-modline to play well with projectile
(setq doom-modeline-project-detection 'projectile)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)

;; Setting up icon configuration
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-state-icon nil)

;; Setting up version control
(setq doom-modeline-vcs-max-length 12)
