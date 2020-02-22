;; Installing packages
(use-package python-mode)
(use-package blacken)
(use-package pyvenv)

;; NOTE: If you're getting a jediepcserver error, then make a Python2 virtual
;;       environment:
;;
;;   - rm -rf ~/.emacs.d/.python-environments/default
;;   - virtualenv -p ~/.emacs.d/.python-environments/default

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
  (setup-venv)
  (ensure-black)
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

;; Skipping string normalization for Black.
(setq blacken-skip-string-normalization t)
