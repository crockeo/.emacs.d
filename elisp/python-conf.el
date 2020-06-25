;; Installing packages
(use-package blacken)

(use-package lsp-python-ms
  :after lsp-mode)

(use-package pyvenv)

(defun python-conf--install-packages (&rest package-names)
  "Installs a series of python packages with `pip` in the current environment."
  (apply 'start-process
         "python-conf--install-package" "python-conf--install-package"
         "pip"
         "install"
         package-names))

(defun python-conf--enter-venv ()
  "Enters a virtual Python environment in the current project directory if it
exists and is not currently active."
  (when (or (not pyvenv-virtual-env-name)
            (not (equal pyvenv-virtual-env-name
                        (projectile-project-name))))
    (let ((venv (concat (projectile-project-root) "venv")))
      (when (file-exists-p venv)
        (progn
          (pyvenv-activate venv)
          t)))))

(defun python-conf--ensure-packages ()
  "Ensures we have the required pip-installable packages."
  (apply 'python-conf--install-packages
         (seq-filter
          (lambda (package)
            (not (executable-find package)))
          '("black" "flake8"))))

(defun python-conf--squelch-eldoc ()
  "Squelches errors we receive from eldoc"
  (setq eldoc-documentation-function (lambda (&rest args) nil)))

(defun python-conf--use-flake8 ()
  "Forces Emacs to use flake8 with flycheck even while using the Microsoft
Python LSP server."
  (setq flycheck-python-flake8-executable "flake8")
  (flycheck-select-checker 'python-flake8)
  (flycheck-mode t))

(defun python-conf--setup ()
  "Sets up Python config when we enter the Python major mode"
  (when (python-conf--enter-venv)
    (python-conf--ensure-packages))

  (python-conf--squelch-eldoc)
  (python-conf--use-flake8)
  (setq lsp-enable-indentation nil))

(add-hook 'python-mode-hook 'python-conf--setup)

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
