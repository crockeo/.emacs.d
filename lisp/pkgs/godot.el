;;; godot.el -*- lexical-binding: t; -*-

(ch/pkg godot
  (use-package gdscript-mode
    :straight (gdscript-mode
               :type git
               :host github
               :repo "godotengine/emacs-gdscript-mode"))

  (add-hook 'gdscript-mode-hook #'lsp-deferred))
