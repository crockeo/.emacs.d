(ch/pkg projectile ()
  (use-package projectile
    :init
    (setq projectile-project-search-path '("~/src/" "~/src/personal"))
    (projectile-discover-projects-in-search-path))

  (use-package counsel-projectile
    :after (ivy projectile)))
