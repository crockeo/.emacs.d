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
            (message "Virtualenv activated in '%s'" venv))
        (message "No virtualenv found at '%s'" venv)))))

;; Ensuring we have black installed.
(defun ensure-black ()
  (unless (executable-find "black")
    (progn
      (start-process "ensure-black" "ensure-black"
                     "pip"
                     "install"
                     "black")
      (message "black installed"))))

(defun ensure-lsp ()
  "Ensures that we have a language server installed in the project. We make sure
  this executes within the venv, so that we "
  (unless (executable-find "pyls")
    (progn
      (start-process "ensure-pyls" "ensure-pyls"
                     "pip"
                     "install"
                     "python-language-server[all]")
      (message "pyls installed"))))

(defun ensure-packages ()
  "Ensures that we have all of the packages we expect installed in any Python
  environment."
  (ensure-black)
  (ensure-lsp))

;; Setting company-jedi with company-mode
(defun setup-python-mode ()
  (setup-venv)
  (ensure-packages)
  (define-key python-mode-map (kbd "RET") 'newline-and-indent)
  (setq lsp-enable-indentation nil)
  (message "Python mode setup complete"))

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

;; Making our max line length longer for lsp-pyls
(setq lsp-pyls-plugins-pycodestyle-max-line-length 120)

;; Intercepting calls to py-help-at-point so that we never have to see
;; *Python-Help* ever again.
(defun intercept-call (oldfn &rest args))
(advice-add 'py-help-at-point
            :around #'intercept-call)
