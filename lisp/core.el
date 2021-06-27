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

(defun ch/pkg/do-once (thing)
  (let ((done nil))
    (lambda (&rest args)
      (unless done
	(setq done t)
	(apply thing args)))))

(defun ch/pkg/new-hooks (old-hook hook)
  (seq-filter
   (lambda (element)
     (not (member element old-hook)))
   hook))

(defun ch/pkg/run-and-new-hooks (hook once-body)
  (let ((old-hook (symbol-value hook)))
    (funcall once-body)
    (dolist (new-hooks (ch/pkg/new-hooks old-hook (symbol-value hook)))
      (funcall new-hooks))))

(defun ch/pkg/impl (name hooks body)
  (unless (assoc name ch/pkg-list)
    ;; If there are no hooks defined
    ;; just load when specified by use-pkgs
    (when (= (length hooks) 0)
      (push `(,name . ,body) ch/pkg-list))

    ;; If there are hooks defined,
    ;; instead register a lambda
    ;; that adds the body to the hooks
    ;; but has it only run once
    (when (not (= (length hooks) 0))
      (let* ((once-body (ch/pkg/do-once body))
	     (add-hooks (lambda ()
			  (dolist (hook hooks)
			    (add-hook hook
				      (lambda ()
					(ch/pkg/run-and-new-hooks hook once-body)))))))
	(push `(,name . ,add-hooks) ch/pkg-list)))))

(defmacro ch/pkg (name hooks &rest body)
  (declare (indent defun))
  `(ch/pkg/impl ',name ',hooks (lambda () ,@body)))

(defun ch/pkg-lang/impl (name file-suffixes body)
  (let* ((mode-name (concat (symbol-name name) "-bootstrapping-mode"))
	 (hook-name (concat mode-name "-hook")))
    (eval `(define-derived-mode ,(intern mode-name) fundamental-mode ""))
    (dolist (file-suffix file-suffixes)
      (push (cons file-suffix (intern mode-name)) auto-mode-alist))
    (ch/pkg/impl name `(,(intern hook-name)) body)))

(defmacro ch/pkg-lang (name file-suffixes &rest body)
  (declare (indent defun))
  `(ch/pkg-lang/impl ',name ',file-suffixes (lambda () ,@body)))

(defun ch/use-pkgs-impl (packages)
  (dolist (package packages)
    (funcall (cdr (assoc package ch/pkg-list)))))

(defmacro ch/use-pkgs (&rest packages)
  (declare (indent defun))
  `(ch/use-pkgs-impl ',packages))
