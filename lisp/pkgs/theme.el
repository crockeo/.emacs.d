;;; theme.el -*- lexical-binding: t; -*-

(ch/pkg theme
  (use-package modus-themes
    :config
    (modus-themes-load-themes)
    (setq modus-themes-hl-line '(accented)))

  (use-package circadian
    :config
    (require 'solar)
    (setq calendar-latitude 47.6
          calendar-longitude -122.3)
    (setq circadian-themes '((:sunrise . modus-operandi)
                             (:sunset  . modus-vivendi)))

    (solar-setup)
    (circadian-setup)))
