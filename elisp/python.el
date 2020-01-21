;; Setting company-jedi with company-mode
(defun add-company-jedi ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'add-company-jedi)
