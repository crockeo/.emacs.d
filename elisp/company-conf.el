;; Installing packages
(use-package company
  :init (global-company-mode))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode))

;; Until Emacs27 is updated on macOS, you need to use this revision of company-posframe, or else
;; calls to `lower-frame` are going to hide emacs.
;;
;; https://github.com/tumashu/company-posframe/tree/4e8f2056fa71aa5749341d834226e3ec786cea63

;; Forcing company to be case-sensitive
(defvar company-dabbrev-downcase)
(setq company-dabbrev-downcase nil)

;; Making auto-complete appear while typing, instead of having to wait.
(setq company-idle-delay 0.075)
(setq company-quickhelp-delay 0.001)

;; Matching quickhelp colors to theme colors
(require 'hawaii-theme)

(setq company-quickhelp-color-background hawaii-background-light)
(setq company-quickhelp-color-foreground hawaii-text)

;; Allowing me to type things that are not autocomplete
(setq company-require-match nil)
