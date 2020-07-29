;;; git-package.el
;;
;; Author: Cerek Hillen <cerekh@gmail.com>
;; Keywords: git, use-package
;;
;;; Commentary:
;;
;; Provides a familiar interface for use-package via directly cloning a repo from GitHub.
;;
;;; Code:

(defconst elpa-directory "~/.emacs.d/elpa")

(defmacro git-package--with-directory (directory-name &rest body)
  (declare (indent defun))
  `(let ((cwd default-directory))
     (cd ,directory-name)
     (let ((ret-val ,@body))
       (cd cwd)
       ret-val)))

(defun git-package--exists (git-repo-name)
  (git-package--with-directory elpa-directory
    (file-directory-p git-repo-name)))

(defun git-package--clone (git-repo-name git-repo)
  (unless (git-package--exists git-repo-name)
    (git-package--with-directory elpa-directory
      (start-process "git-clone-package" "git-clone-package"
                     "git" "clone" git-repo))))

(defmacro git-package (git-repo-name git-repo &rest config)
  `(progn
     (git-package--clone ,(symbol-name git-repo-name) ,git-repo)
     (use-package ,git-repo-name
       ,@config)))

(provide 'git-package)
;;; git-package.el ends here
