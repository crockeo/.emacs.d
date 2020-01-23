;; Setting company-jedi with company-mode
(defun add-company-jedi ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'add-company-jedi)

;; Making jedi try to autocomplete at the end of a dot.
(setq jedi:complete-on-dot t)

;; Auto-formatting with Black whenever we save.
(defun register-hooks ()
  (add-hook 'before-save-hook 'blacken-buffer))

(add-hook 'python-mode-hook 'register-hooks)
