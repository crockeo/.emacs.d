;;; doom-modeline.el

(ch/pkg doom-modeline
  (use-package doom-modeline
    :config
    (setq doom-modeline-env-version nil)
    (setq inhibit-compacting-font-caches t)

    :custom
    (doom-modeline-icon nil)

    :hook (after-init . doom-modeline-mode)))
