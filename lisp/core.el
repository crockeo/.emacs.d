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

;; Set up some commonly used helper functions.
(defun ch/vec-to-list (vec)
  "Capturing this little hack I got from @neeasade
in a self-documenting function."
  (append vec nil))

(defmacro ch/callproc (&rest procs)
  "A simpler version of call-process which doesn't have any of the \
very valuable cruft that I don't need."
  `(progn
     ,@(-map
	(lambda (args) `(call-process ,(car args) nil nil nil ,@(cdr args)))
	procs)))

(defmacro ch/under-dir (dir &rest body)
  "Perform some action while assuming the directory `dir`."
  (declare (indent defun))
  `(let ((pwd default-directory)
	 (result-slot nil))
     (condition-case err
	 (progn
	   (cd ,dir)
	   (setq result-slot
	    (progn ,@body))
	   (cd pwd)
	   result-slot)
       (error
	(progn
	  (cd pwd)
	  (signal (car err) (cdr err)))))))

(defmacro ch/comment (&rest body)
  (declare (indent defun))
  nil)
