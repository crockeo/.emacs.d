;;; ivy.el -*- lexical-binding: t; -*-


(ch/pkg ivy
  (use-package ivy
    :init (ivy-mode 1)
    :config
    (define-key ivy-minibuffer-map
      (kbd "S-SPC")
      (lambda () (interactive) (insert " "))))

  (use-package counsel
    :after ivy))
