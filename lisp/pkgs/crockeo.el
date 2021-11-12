;;; crockeo.el -*- lexical-binding: t; -*-

(ch/pkg crockeo
  (defvar crockeo-mode-map (make-sparse-keymap))

  (define-minor-mode crockeo-mode
    ""
    nil
    " crockeo"
    :keymap crockeo-mode-map
    :global t)

  (defun ch/crockeo/register-keys/impl (keymap)
    (define-key crockeo-mode-map
      (kbd (car keymap))
      (cdr keymap)))

  (defmacro ch/crockeo/register-keys (&rest keymaps)
    (declare (indent defun))
    `(mapc #'ch/crockeo/register-keys/impl ',keymaps))

  (ch/crockeo/register-keys
    ("C-c C-w C-h" . ch/org/home/go-home)
    ("C-c C-w C-w" . ch/org/home/go-week)
    ("C-c C-w C-t" . ch/org/home/go-today)
    ("C-c C-w C-b" . ch/org/home/go-back))

  (crockeo-mode 1))
