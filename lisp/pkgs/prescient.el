;;; prescient.el -*- lexical-binding: t; -*-

(ch/pkg prescient
  (use-package prescient)

  (use-package company-prescient
    :after (company prescient)
    :hook (company-mode . company-prescient-mode))

  (use-package ivy-prescient
    :after (counsel ivy prescient)
    :hook (ivy-mode . ivy-prescient-mode)))
