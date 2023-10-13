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
    doom-modeline
    dtrt-indent
    ivy
    magit
    notion
    olivetti
    ;; theme has to sit before org
    ;; because we use modus-themes functions
    theme
    ;; org has to sit before copilot
    ;; because copilot imports org
    ;; and if copilot imports the built-in org
    ;; then emacs gets sad :(
    org
    projectile
    vterm
    which-key
    winconf))

(ch/pkg languages
  (ch/use-pkgs
    treesitter

    bazel
    clojure
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
