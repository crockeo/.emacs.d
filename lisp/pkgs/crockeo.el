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
    ("C-c C-w C-b" . org-roam-dailies-goto-previous-note)
    ("C-c C-w C-d" . ch/org/org-roam-dailies-goto-today)
    ("C-c C-w C-f" . ch/org/org-roam-node-find)
    ("C-c C-w C-j" . ch/org/org-roam-dailies-goto-date)
    ("C-c C-w C-m" . ch/org/ch/org/roam-metaproject-find)
    ("C-c C-w C-n" . org-roam-dailies-goto-next-note)
    ("C-c C-w C-p" . ch/org/ch/org/roam-project-find)
    ("C-c C-w C-q" . ch/org/pop-winconf)
    ("C-c C-w C-t" . ch/org/org-roam-dailies-goto-tomorrow)
    ("C-c C-w C-y" . ch/org/org-roam-dailies-goto-yesterday)
    )

  (crockeo-mode 1))
