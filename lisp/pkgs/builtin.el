(ch/pkg builtin ()

  
  (set-face-attribute 'default nil
		      :font "Fira Mono"
		      :height 140)
  
  (setq gc-cons-threshold (* 1024 1024 1024 2) ;; 2GB
	inhibit-startup-screen t
	ring-bell-function 'ignore)

  (global-display-line-numbers-mode 1))
