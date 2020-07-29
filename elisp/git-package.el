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

(defmacro git-package (package-name git-repo &rest config)
  (declare (indent defun))
  `(progn
     (quelpa '(,package-name
               :repo ,git-repo
               :fetcher github-ssh))
     (use-package ,package-name
       ,@config)))

(provide 'git-package)
;;; git-package.el ends here
