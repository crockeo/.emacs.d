;;; load-pkgs.el -*- lexical-binding: t; -*-


(dolist (file (directory-files "~/.emacs.d/lisp/pkgs" t "\\.el$"))
  (load file))

(ch/pkg interface
  (ch/use-pkgs
    colorscheme
    company
    evil
    flycheck
    diff-hl
    dired-sidebar
    ivy
    magit
    projectile
    vterm))

(ch/pkg languages
  (ch/use-pkgs
    go
    lisp
    org
    proto
    python
    yaml))

(ch/use-pkgs
  builtin
  interface
  languages
  lsp)
