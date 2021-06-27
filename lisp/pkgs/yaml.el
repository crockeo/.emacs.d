;;; yaml.el -*- lexical-binding: t; -*-

(define-derived-mode yaml-bootstrap-mode fundamental-mode
  "Major mode used for bootstrapping yaml packaging.")

(push '("\\.yaml\\'" . yaml-bootstrap-mode) auto-mode-alist)

(ch/pkg yaml (yaml-bootstrap-mode-hook)
  (use-package yaml-mode))
