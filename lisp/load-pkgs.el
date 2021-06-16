;;; load-pkgs.el -*- lexical-binding: t; -*-


(dolist (file (directory-files "~/.emacs.d/lisp/pkgs" t "\\.el$"))
  (load file))


(ch/pkg interface ()
  (ch/use-pkgs
    colorscheme
    evil
    ivy))

(ch/pkg languages ()
  (ch/use-pkgs
    lisp
    org))


(ch/use-pkgs
  interface
  languages)

