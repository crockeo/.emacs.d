;;;; failing-tests.el
;;;;
;;;; My job renders Jenkins CI on GitHub, where we're shown succeeding / failing tests. We also have
;;;; our tests shareded, and a -vvv flag enabled for Pytest.
;;;;
;;;; This makes it really annoying to find the complete set of failing tests on a CI build. This
;;;; script:
;;;;
;;;;   1. Auths into GH with OAuth
;;;;   2. Auths into my company's GH with OAuth
;;;;   3. Fetches failing tasks on a GitHub PR
;;;;   4. Parses the resultent console output to find the set of failing tests.

(require 'rx)

;;;
;;; This section fetches the failnig tests from a specific job on CI.
;;;

(defvar pytest-vvv-failed-test-regexp
  (rx
   bol
   (+ (+ digit)
      (? (any ?: ?.)))
   space
   (group-n 1
            (+ (not space)))
   (optional ?\[
             (+ (not space))
             ?\])
   space
   "FAILED"))

(defun url-retrieve-as-string (url)
  "Retrieves the contents of a webpage and returns it as a string."
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((contents (buffer-string)))
      (kill-buffer)
      contents)))

(defun split-and-filter (str)
  "Splits a string into lines, filters out only the lines that are a failing pytest (at -vvv), and
transforms them into only the test name."
  (let ((str-lines (split-string str "\n")))
    (loop for str-line in str-lines
          if (string-match "FAILED \\[" str-line)
          collect (progn
                    (string-match pytest-vvv-failed-test-regexp str-line)
                    (substring str-line
                               (match-beginning 1)
                               (match-end 1))))))

(defun failing-tests (url)
  "Returns all of the failing tests from the provided URL."
  (let ((test-names (split-and-filter (url-retrieve-as-string url))))
    (delete-dups test-names)
    test-names))

;;;
;;; This section performs GitHub auth and queries the GH API.
;;;

(defvar gh-access-token-path "~/.emacs.d/secret/gh-auth")
(defvar gh-api-root "https://api.github.com")

(defun load-personal-access-token ()
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents gh-access-token-path)
        (buffer-string))
    (error nil)))

(defun make-auth-token (username)
  (let ((access-token (load-personal-access-token)))
    (base64-encode-string (format "%s:%s" username access-token))))

(defun parse-json-body (response)
  (when (string-match (rx
                       "\n\n"
                       (group-n 1 (+ anything)))
                      response)
    (json-parse-string (substring response
                                  (match-beginning 1)
                                  (match-end 1)))))

(defmacro gh-request (username method url)
  `(let ((url (string-join (list gh-api-root ,@url) "/"))
         (url-request-extra-headers `(("Accept" . "application/vnd.github.antiope-preview+json")
                                      ("Authorization" . ,(format "Basic %s" (make-auth-token ,username)))))
         (url-request-method ,method))
     (condition-case nil
         (parse-json-body (url-retrieve-as-string url))
       (error nil))))

(defun get-statuses-for-ref (owner repo ref)
  (let ((response (gh-request "crockeo"
                              "GET"
                              ("repos" owner repo "commits" ref "status"))))
    (when response
      (gethash "statuses" response))))

(defun get-failed-status-urls-for-ref (owner repo ref)
  (let ((statuses (get-statuses-for-ref owner repo ref)))
    (when statuses
      (mapcar
       (lambda (status)
         (gethash "target_url" status))
       (remove-if-not
        (lambda (status)
          (equal (gethash "state" status) "failure"))
        statuses)))))

(defun get-head-of-pr (owner repo pr)
  (let ((response (gh-request "crockeo"
                              "GET"
                              ("repos" owner repo "pulls" pr))))
    (when response
      (gethash "sha" (gethash "head" response)))))

(defun get-failed-status-urls-for-pr (owner repo pr)
  (let ((sha (get-head-of-pr owner repo pr)))
    (get-failed-status-urls-for-ref owner repo sha)))

;;;
;;; This section handles the interactive command executes the GH / test extraction code.
;;;

(defun get-failed-tests-for-pr (owner repo pr)
  (let* ((failed-status-urls (get-failed-status-urls-for-pr owner repo pr))
         (failed-tests (mapcar
                        (lambda (url)
                          (failing-tests url))
                        failed-status-urls)))
    (delete-dups (apply #'append failed-tests))))
