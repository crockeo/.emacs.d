;;; copilot.el -*- lexical-binding: t; -*-

(ch/pkg copilot
  (use-package copilot
    :after org
    :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
    :ensure t
    :bind
    (("C-c a" . copilot-mode))))
