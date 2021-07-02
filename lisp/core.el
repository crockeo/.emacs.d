;;; core.el -*- lexical-binding: t; -*-


;; Set up package management. Using straight.el because it pins dependencies OOTB.
(defvar bootstrap-version)
(let ((bootstrap-file
    (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
    (bootstrap-version 5))
(unless (file-exists-p bootstrap-file)
(with-current-buffer
    (url-retrieve-synchronously
	"https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
(load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; My own custom stuff :)
(defvar ch/pkg-list '())

(defun ch/pkg/impl (name body)
  (unless (assoc name ch/pkg-list)
    (push (cons name body) ch/pkg-list)))

(defmacro ch/pkg (name &rest body)
  (declare (indent defun))
  `(ch/pkg/impl ',name (lambda () ,@body)))

(defun ch/use-pkgs-impl (packages)
  (dolist (package packages)
    (funcall (cdr (assoc package ch/pkg-list)))))

(defmacro ch/use-pkgs (&rest packages)
  (declare (indent defun))
  `(ch/use-pkgs-impl ',packages))
