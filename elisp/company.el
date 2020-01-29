(require 'company)
(require 'company-quickhelp)

;; Forcing company to be case-sensitive
(defvar company-dabbrev-downcase)
(setq company-dabbrev-downcase nil)

;; Making auto-complete appear while typing, instead of having to wait.
(setq company-idle-delay 0.001)
(setq company-quickhelp-delay 0.001)

(setq company-quickhelp-color-background "#272822")
(setq company-quickhelp-color-foreground "#F8F8F2")

;; Allowing me to type things that are not autocomplete
(setq company-require-match nil)
