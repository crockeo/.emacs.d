;;; load-pkgs.el -*- lexical-binding: t; -*-


(dolist (file (directory-files "~/.emacs.d/lisp/pkgs" t "\\.el$"))
  (load file))

(ch/pkg interface ()
  (ch/use-pkgs
    colorscheme
    company
    evil
    flycheck
    ivy
    projectile))

(ch/pkg languages ()
  (ch/use-pkgs
    lisp
    org))

(ch/use-pkgs
  builtin
  interface
  languages
  lsp)
