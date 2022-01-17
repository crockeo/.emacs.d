;;; applescript.el -*- lexical-binding: t; -*-

(ch/pkg applescript
  (defun applescript-command--eval-pair (pair)
    (let ((key (applescript-command--impl (car pair)))
	  (value (applescript-command--impl (cdr pair))))
      `(format "%s:%s" ,key ,value)))

  (defun applescript-command--impl (form)
    (pcase form
      (`(:tell ,application . ,body)
       `(string-join (list (format "tell application %s"
				   ,(applescript-command--impl application))
			   ,@(-map #'applescript-command--impl body)
			   "end tell")
		     "\n"))

      (`(:if ,cond . ,body)
       `(string-join (list (format "if %s then"
				   ,(applescript-command--impl cond))
			   ,@(-map #'applescript-command--impl body)
			   "end if")
		     "\n"))

      (`(:dict . ,body)
       `(concat "{"
		(string-join (list ,@(-map #'applescript-command--eval-pair body))
			     ", ")
		"}"))

      (`(: . ,body)
       `(string-join (list ,@(-map #'applescript-command--impl body))
		     " "))

      (`(,keyword . ,rest)
       (if (and (symbolp keyword)
		(s-starts-with-p ":" (symbol-name keyword)))
	   (error (format "Unrecognized form: %s" (cons keyword rest)))
	 (cons keyword rest)))

      (form form)))

  (defmacro applescript-command (form)
    (applescript-command--impl form))

  (defmacro do-applescript-command (form)
    `(do-applescript (applescript-command ,form))))
