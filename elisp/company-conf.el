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

;; Until Emacs27 is updated on macOS, it causes the entire window to be hidden under the application
;; beneath it. This code snipped just prevents lower-frame from actually being executed.
(defun no-lower-frame (orig-fun &rest args)
  nil)

(advice-add
 'lower-frame
 :around #'no-lower-frame)
