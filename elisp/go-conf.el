;; Installing packages
(use-package go-mode
  :ensure t)

(use-package company-go
  :after company go-mode
  :ensure t)

;; Customizing indentation in golang to be tabs of width 4.
(defun custom-indentation ()

  (setq indent-tabs-mode 1)
  (setq tab-width 4))

(add-hook 'go-mode-hook 'custom-indentation)
