(require 'company)
(require 'jedi-core)

;; Setting company-jedi with company-mode
(defun setup-python-mode ()
  ;; Letting me use my comment command
  (local-unset-key (kbd "C-c C-c"))

  ;; Making jedi launch
  (add-to-list 'company-backends 'company-jedi)
  (jedi-mode)

  ;; Formatting file with black on save
  (add-hook 'before-save-hook 'blacken-buffer))

(add-hook 'python-mode-hook 'setup-python-mode)

;; Fine-tuning jedi autocomplete
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 250)
