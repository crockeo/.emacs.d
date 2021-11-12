;;; load-pkgs.el -*- lexical-binding: t; -*-


(dolist (file (directory-files "~/.emacs.d/lisp/pkgs" t "\\.el$"))
  (load file))

(ch/pkg interface
  (ch/use-pkgs
    colorscheme
    crockeo
    company
    evil
    flycheck
    diff-hl
    dired-sidebar
    ivy
    magit
    olivetti
    projectile))

(ch/pkg languages
  (ch/use-pkgs
    bazel
    cpp
    go
    javascript
    lisp
    org
    proto
    python
    rust
    swift
    yaml))

(ch/use-pkgs
  builtin
  interface
  languages
  lsp)
