(require 'company)
(require 'elpy)
(require 'jedi-core)
(require 'projectile)
(require 'pyvenv)

;; Automatically switches to the venv in a Python directory if it exists.
(defun setup-venv ()
  (let ((venv (concat (projectile-project-root) "venv")))
    (if (file-exists-p venv)
        (pyvenv-activate venv))))

;; Ensuring we have black installed.
(defun ensure-black ()
  (if (not (executable-find "black"))
      (shell-command "pip install black &")))

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
