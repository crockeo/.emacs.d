;;; theme.el -*- lexical-binding: t; -*-

(ch/pkg theme
  (use-package modus-themes
    :config
    (setq modus-themes-common-palette-overrides
	  '((bg-main "#222222")
	    ,@modus-themes-preset-overrides))
    (modus-themes-load-theme 'modus-vivendi)))
