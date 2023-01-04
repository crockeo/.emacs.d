;;; theme.el -*- lexical-binding: t; -*-

(ch/pkg theme
  (use-package modus-themes
    :config
    (modus-themes-load-themes)
    (setq modus-themes-hl-line '(accented))
    (setq modus-themes-vivendi-color-overrides
	  '((bg-main . "#222222"))))

  (use-package circadian
    :config
    (require 'solar)

    ;; Seattle
    ;;
    (setq calendar-latitude 47.6
          calendar-longitude -122.3)
    ;;
    ;; Amsterdam
    ;;
    ;; (setq calendar-latitude 52.4
    ;; 	  calendar-longitude 4.9)
    ;;
    ;; Gdansk
    ;;
    ;; (setq calendar-latitude 54.4
    ;; 	  calendar-longitude 18.6)

    (setq circadian-themes
	  '(
	    ;; NOTE: trying out dark mode-only for a little while
	    ;; to see if it helps with my eye-fatigue
	    ;; (:sunrise . modus-operandi)
	    (:sunrise . modus-vivendi)
            (:sunset  . modus-vivendi)
	    ))

    (solar-setup)
    (circadian-setup)))
