;;; ivy.el -*- lexical-binding: t; -*-


(ch/pkg ivy ()
  (use-package ivy
    :init (ivy-mode 1))

  (use-package prescient)

  (use-package ivy-prescient
    :after (ivy prescient)
    :hook (ivy-mode . ivy-prescient-mode)))
