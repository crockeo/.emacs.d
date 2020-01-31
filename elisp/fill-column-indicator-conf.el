;; Packages
(use-package fill-column-indicator
  :ensure t)

;; Defining a global fci
(define-global-minor-mode global-fci-mode
  fci-mode
  (lambda ()
    (fci-mode 1)))

;; Disabling fci when doing company completion, so that we don't have company
;; render on the right hand side of the fill indicator.
(defvar-local fci-was-on nil)

(defun disable-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq fci-was-on fci-mode)
    (when fci-mode (global-fci-mode 0))))

(defun re-enable-fci (&rest ignore)
  (when fci-was-on
    (global-fci-mode 1)))

(add-hook 'company-completion-started-hook 'disable-fci)
(add-hook 'company-completion-finished-hook 're-enable-fci)
(add-hook 'company-completion-cancelled-hook 're-enable-fci)

;; Sets the fill column to be 120 instead of the more aggressive 70.
(setq-default fill-column 120)

;; Enables by default
(global-fci-mode 1)
