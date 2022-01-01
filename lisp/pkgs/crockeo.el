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
    ("C-c C-w C-q" . ch/org/agenda/pop-winconf)

    ("C-c C-w C-d" . ch/org/agenda/go-day)
    ("C-c C-w C-f" . ch/org/agenda/go-roam-find)
    ("C-c C-w C-h" . ch/org/agenda/go-home)
    ("C-c C-w C-r" . ch/org/agenda/go-recent)
    ("C-c C-w C-t" . ch/org/agenda/go-todo)
    ("C-c C-w C-w" . ch/org/agenda/go-week)
    ("C-c C-w C-y" . ch/org/agenda/go-yesterday))

  (crockeo-mode 1))
