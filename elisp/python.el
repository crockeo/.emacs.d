(require 'company)
(require 'elpy)

;; Setting company-jedi with company-mode
(defun setup-python-mode ()
  ;; Launching elpy
  (elpy-mode)

  ;; Formatting file with black on save
  (add-hook 'before-save-hook 'blacken-buffer))

(add-hook 'python-mode-hook 'setup-python-mode)
