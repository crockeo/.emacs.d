;; Installing packages
(use-package python-mode
  :ensure t)

(use-package elpy
  :ensure t)

(use-package blacken
  :ensure t)

(require 'company)
(require 'jedi-core)
(require 'projectile)

(load-library "jedi-posframe")

;; Installing elpy dependencies in the virtualenv.
(defun setup-venv-elpy ()
  (apply 'start-process
         "setup-venv-elpy" "setup-venv-elpy"
         "pip"
         "install"
         "--upgrade"
         (elpy-rpc--get-package-list)))

;; Automatically switches to the venv in a Python directory if it exists.
(defun setup-venv ()
  (unless pyvenv-virtual-env-name
    (let ((venv (concat (projectile-project-root) "venv")))
      (if (file-exists-p venv)
          (progn
            (pyvenv-activate venv)
            (message "Virtualenv activated in '%s'" venv)

            (setup-venv-elpy)
            (message "Elpy dependencies installed in '%s'" venv))
        (message "No virtualenv found at '%s'" venv)))))

;; Ensuring we have black installed.
(defun ensure-black ()
  (unless (executable-find "black")
    (progn
      (start-process "ensure-black" "ensure-black"
                     "pip"
                     "install"
                     "black")
      (message "Black installed"))))

;; Setting company-jedi with company-mode
(defun setup-python-mode ()
  ;; Setting up out virtualenv
  (setup-venv)

  ;; Launching jedi
  (elpy-mode)
  (jedi-mode)
  (add-to-list 'company-backends 'company-jedi)

  ;; Making jedi render with posframe
  (jedi-posframe-mode)

  ;; Removing highlight-indentation-mode from elpy
  (highlight-indentation-mode 0)

  ;; Configuring black
  (ensure-black)

  ;; Keeping Python mode from rebinding my precious RET
  (define-key python-mode-map (kbd "RET") 'newline-and-indent))

(add-hook 'python-mode-hook 'setup-python-mode)

;; Forcing python-mode to opinionated indentation.
(defun custom-indentation (orig-fun &rest args)
  (save-excursion
    (pcase (python-indent-context)
      ;; When we make a new line in a docstring, we want to move to the
      ;; same indentation level.
      (`(:inside-docstring . ,start)
       (goto-char start)
       (current-indentation))

      ;; When we define a new function, we only want to be one block indented
      ;; from the function definition, not two.
      (`(:inside-paren-newline-start-from-block . ,start)
       (goto-char start)
       (+ (current-indentation) python-indent-offset))

      ;; If we are not in an overriden context, execute the standard function.
      (_ (apply orig-fun args)))))

(advice-add
 'python-indent-calculate-indentation
 :around #'custom-indentation)

;; Configuring jedi to work within elpy.
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 250)
