;; Installing packages
(use-package blacken)
(use-package pyvenv)

(defun python-conf--install-packages (&rest package-names)
  "Installs a series of python packages with `pip` in the current environment."
  (apply 'start-process
         "python-conf--install-package" "python-conf--install-package"
         "pip"
         "install"
         package-names))

;; Automatically switches to the venv in a Python directory if it exists.
(defun setup-venv ()
  (if pyvenv-virtual-env-name
      t
    (let ((venv (concat (projectile-project-root) "venv")))
      (if (file-exists-p venv)
          (progn
            (pyvenv-activate venv)
            t)
        nil))))

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
      (python-conf--install-packages "python-language-server[all]")
      (message "pyls installed"))))

(defun ensure-packages ()
  "Ensures that we have all of the packages we expect installed in any Python
  environment."
  (ensure-black)
  (ensure-lsp))

(defun squelch-eldoc ()
  (setq eldoc-documentation-function
        (lambda (&rest args)
          nil)))

(defun setup-python-mode ()
  (if (setup-venv)
      (progn
        (ensure-packages)
        (squelch-eldoc)
        (setq lsp-enable-indentation nil)
        (message "Python mode setup complete"))
    (error "No venv available")))

(add-hook 'python-mode-hook 'setup-python-mode)

(defun custom-indentation (orig-fun &rest args)
  "Forces python-mode to have sane indentation when writing docstrings"
  (save-excursion
    (pcase (python-indent-context)
      ;; When we make a new line in a docstring, we want to move to the
      ;; same indentation level.
      (`(:inside-docstring . ,start)
       (goto-char start)
       (current-indentation))

      ;; If we are not in an overriden context, execute the standard function.
      (_ (apply orig-fun args)))))

(advice-add
 'python-indent-calculate-indentation
 :around #'custom-indentation)

;; Skipping string normalization for Black.
(setq blacken-skip-string-normalization t)

;; Removing trailing commas
(setq blacken-allow-py36 t)

;; Making our max line length longer for lsp-pyls
(setq lsp-pyls-plugins-pycodestyle-max-line-length 120)
