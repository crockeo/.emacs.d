;;; colorscheme.el -*- lexical-binding: t; -*-

(ch/pkg colorscheme
  ;; unfortunately i can't have the actual theme here
  ;; because it needs to be discoverable by emacs
  ;; so it's in ~/.emacs.d/ and i just load it here :(
  (load "~/.emacs.d/hawaii-theme")
  (require 'hawaii-theme)
  (load-theme 'hawaii t))
