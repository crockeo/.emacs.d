(require 'go-mode)

;; Customizing indentation in golang to be tabs of width 4.
(defun custom-indentation ()

  (setq indent-tabs-mode 1)
  (setq tab-width 4))

(add-hook 'go-mode-hook 'custom-indentation)
