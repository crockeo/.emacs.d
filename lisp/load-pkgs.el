;;; load-pkgs.el -*- lexical-binding: t; -*-


(dolist (file (directory-files "~/.emacs.d/lisp/pkgs" t "\\.el$"))
  (load file))

(ch/pkg core
  (ch/use-pkgs
    dash
    s
    work))

(ch/pkg interface
  (ch/use-pkgs
    company
    crockeo
    selection
    dired-sidebar
    evil
    flycheck
    diff-hl
    dtrt-indent
    magit
    olivetti
    ;; theme has to sit before org
    ;; because we use modus-themes functions
    theme
    ;; org has to sit before copilot
    ;; because copilot imports org
    ;; and if copilot imports the built-in org
    ;; then emacs gets sad :(
    org
    copilot
    prescient
    project
    which-key
    winconf))

(ch/pkg languages
  (ch/use-pkgs
    treesitter

    bazel
    cpp
    dockerfile
    elixir
    go
    godot
    javascript
    lisp
    markdown
    nix
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
  lsp
  )
