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

;; Skipping string normalization for Black.
(setq blacken-skip-string-normalization t)
