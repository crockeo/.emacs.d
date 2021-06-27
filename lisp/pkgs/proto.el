;;; proto.el -*- lexical-binding: t; -*-

(ch/pkg-lang proto ("\\.proto\\'")
  (use-package protobuf-mode
    :init (protobuf-mode)))
