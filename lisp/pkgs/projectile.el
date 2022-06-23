(ch/pkg projectile
  (use-package projectile
    :init
    (setq projectile-project-search-path
	  (->> '("~/src/" "~/src/personal/" "~/src/tmp/")
	    (-map #'expand-file-name)
	    (-filter #'file-exists-p)))
    (projectile-discover-projects-in-search-path))

  (use-package counsel-projectile
    :after (ivy projectile)
    :config
    (setq counsel-ag-base-command '("ag" "--hidden" "--vimgrep" "%s" "."))))
