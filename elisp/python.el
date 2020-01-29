(require 'company)
(require 'elpy)
(require 'jedi-core)
(require 'projectile)
(require 'pyvenv)

;;;;
;; TODO:
;;   * Indentation
;;     * Make python-mode indent only one block, instead of 2 blocks, when in a
;;       function declaration.
;;
;;     * Make python-mode indent to the last indentation level inside of a
;;       multi-line string.

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

  ;; Configuring black
  (ensure-black))

(add-hook 'python-mode-hook 'setup-python-mode)

;; Configuring jedi to work within elpy.
(setq jedi:complete-on-dot t)
(setq jedi:get-in-function-call-delay 250)
