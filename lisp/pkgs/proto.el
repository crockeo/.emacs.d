;;; proto.el -*- lexical-binding: t; -*-

(define-derived-mode proto-bootstrap-mode fundamental-mode
  "Major mode used for bootstrapping protobuf packaging.")

(push '("\\.proto\\'" . proto-bootstrap-mode) auto-mode-alist)

(ch/pkg proto (proto-bootstrap-mode-hook)
  (use-package protobuf-mode))
