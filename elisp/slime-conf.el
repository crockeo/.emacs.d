(defun find-program (name)
  "Finds the full path of a program using 'where'"
  (car
   (split-string
    (shell-command-to-string (concat "where " name))
    "\n")))


(use-package slime
  :config
  (setq slime-lisp-implementations
        `((sbcl (,(find-program "sbcl") "--dynamic-space-size" "2GB")))))
