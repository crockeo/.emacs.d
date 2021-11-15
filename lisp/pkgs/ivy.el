;;; ivy.el -*- lexical-binding: t; -*-


(ch/pkg ivy
  (use-package ivy
    :init (ivy-mode 1)
    :config
    (define-key ivy-minibuffer-map
      (kbd "S-SPC")
      (lambda () (interactive) (insert " "))))

  (use-package prescient)

  (use-package ivy-prescient
    :after (ivy prescient)
    :hook (ivy-mode . ivy-prescient-mode)))
