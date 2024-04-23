;;; selection.el -*- lexical-binding: t; -*-

(ch/pkg selection
  "
  Provides packages that help with selecting and finding text.
  E.g. finding files in a project, choosing options in a list, etc.
  "

  ;; https://github.com/minad/consult
  (use-package consult
    :bind
    (
     ("C-c p a" . consult-ripgrep)
     ("C-c p b" . consult-project-buffer)
     ("C-c p l" . consult-line)
     ("C-x b" . consult-buffer))

    :init
    (setq consult-async-min-input 0
          consult-async-refresh-delay 0)

    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)
    )

  ;; https://github.com/minad/vertico
  (use-package vertico
    :init
    (vertico-mode)
    (vertico-multiform-mode))
  )
