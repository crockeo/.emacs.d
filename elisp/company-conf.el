;; Installng packages
(use-package company
  :ensure t
  :init (global-company-mode))

(use-package company-quickhelp
  :ensure t
  :hook (company-mode . company-quickhelp-mode))

(use-package company-posframe
  :ensure t
  :hook (company-mode . company-posframe-mode))

;; Forcing company to be case-sensitive
(defvar company-dabbrev-downcase)
(setq company-dabbrev-downcase nil)

;; Making auto-complete appear while typing, instead of having to wait.
(setq company-idle-delay 0.001)
(setq company-quickhelp-delay 0.001)

(message "%s" (cdr (assoc 'background-color (frame-parameters))))

;; Matching quickhelp colors to theme colors
(setq
 company-quickhelp-color-background
 (cdr (assoc 'background-color (frame-parameters))))

(setq
 company-quickhelp-color-background
 (cdr (assoc 'foreground-color (frame-parameters))))

;; Allowing me to type things that are not autocomplete
(setq company-require-match nil)
