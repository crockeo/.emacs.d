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
    vterm
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
    nix
    proto
    python
    rust
    terraform
    yaml
    zig
    ))

(ch/use-pkgs
  builtin
  core
  interface
  languages
  eglot
  )
