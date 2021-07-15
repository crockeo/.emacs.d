;;; bazel-el -*- lexical-binding: t; -*-

(ch/pkg bazel
  (straight-use-package
   '(bazel :type git
	   :host github
	   :repo "bazelbuild/emacs-bazel-mode")
   :config
   (add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-starlark-mode))
   (add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-starlark-mode))))
