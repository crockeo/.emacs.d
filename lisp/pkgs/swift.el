;;; swift.el -*- lexical-binding: t; -*-

(ch/pkg swift
  (defvar
   ch/swift/xcode-toolchain
   (string-trim (shell-command-to-string "xcode-select --print-path")))

  (defun ch/swift/sourcekit-lsp-path ()
    (concat
     ch/swift/xcode-toolchain
     "/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

  (use-package swift-mode
    :hook (swift-mode . (lambda () (lsp))))

  (use-package lsp-sourcekit
    :after swift-mode
    :config
    (setq lsp-sourcekit-executable (ch/swift/sourcekit-lsp-path))))
