;; Installing packages
(use-package company
  :init (global-company-mode)
  :config

  ;; Make company case-sensitive
  (setq company-dabbrev-downcase nil)

  ;; Reduce delay before an autocomplete suggestion
  (setq company-idle-delay 0.075)

  ;; Reduce minimum # of characters before autocomplete to 1
  (setq company-minimum-prefix-length 1)

  ;; Allow me to type non-matched words
  (setq company-require-match nil))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :config

  (require 'hawaii-theme)

  (setq company-quickhelp-color-background hawaii-background-light)
  (setq company-quickhelp-color-foreground hawaii-text))

(use-package company-posframe
  :hook (company-mode . company-posframe-mode))

;; Until Emacs27 is updated on macOS, you need to use this revision of company-posframe, or else
;; calls to `lower-frame` are going to hide emacs.
;;
;; https://github.com/tumashu/company-posframe/tree/4e8f2056fa71aa5749341d834226e3ec786cea63
