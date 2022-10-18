;;; load-pkgs.el -*- lexical-binding: t; -*-


(dolist (file (directory-files "~/.emacs.d/lisp/pkgs" t "\\.el$"))
  (load file))

(ch/pkg core
  (ch/use-pkgs
    applescript
    dash
    request
    s
    work))

(ch/pkg interface
  (ch/use-pkgs
    crockeo
    company
    evil
    flycheck
    diff-hl
    dired-sidebar
    doom-modeline
    ivy
    magit
    notion
    olivetti
    org
    projectile
    theme
    which-key
    winconf))

(ch/pkg languages
  (ch/use-pkgs
    bazel
    cpp
    dockerfile
    go
    javascript
    lisp
    markdown
    proto
    python
    rust
    swift
    terraform
    yaml
    zig
    ))

(ch/use-pkgs
  builtin
  core
  interface
  languages
  lsp)
