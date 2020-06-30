;; Installing the package
(use-package protobuf-mode)

(defconst protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode nil)))

(defun setup-style ()
  (c-add-style "protobuf-style" protobuf-style t))

(add-hook 'protobuf-mode-hook 'setup-style)
