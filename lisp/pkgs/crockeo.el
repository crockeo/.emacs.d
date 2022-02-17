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
    ("C-c C-w C-d" . ch/org/org-roam-dailies-goto-today/wc)
    ("C-c C-w C-f" . ch/org/org-roam-node-find/wc)
    ("C-c C-w C-h" . ch/org/roam-home/wc)
    ("C-c C-w C-j" . ch/org/org-roam-dailies-goto-date/wc)
    ("C-c C-w C-l" . ch/org/roam-backlog-find/wc)
    ("C-c C-w C-m" . ch/org/roam-metaproject-find/wc)
    ("C-c C-w C-n" . org-roam-dailies-goto-next-note)
    ("C-c C-w C-p" . ch/org/roam-project-find/wc)
    ("C-c C-w C-q" . ch/org/pop-winconf)
    ("C-c C-w C-t" . ch/org/org-roam-dailies-goto-tomorrow/wc)
    ("C-c C-w C-w" . ch/org/go-week/wc)
    ("C-c C-w C-y" . ch/org/go-yesterday/wc)
    )

  (crockeo-mode 1))
