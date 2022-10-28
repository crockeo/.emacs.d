(ch/pkg projectile
  (use-package projectile
    :init
    (setq projectile-project-search-path
	  `((,(expand-file-name "~/src/") . 2)))
    (projectile-discover-projects-in-search-path))

  (use-package counsel-projectile
    :after (ivy projectile)
    :config
    (setq counsel-ag-base-command '("ag" "--hidden" "--vimgrep" "%s" "."))))
