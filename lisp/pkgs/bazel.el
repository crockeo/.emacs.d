;;; bazel-el -*- lexical-binding: t; -*-

(ch/pkg bazel
  (straight-use-package
   '(bazel :type git
	   :host github
	   :repo "bazelbuild/emacs-bazel-mode"))

  (dolist (filename '("BUILD\\'" "BUILD.overlay\\'" "WORKSPACE\\'"))
    (add-to-list 'auto-mode-alist `(,filename . bazel-starlark-mode))))
