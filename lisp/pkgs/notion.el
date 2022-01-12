;;; notion.el -*- lexical-binding: t; -*-

(ch/pkg notion
  (use-package ht)

  ;; a bunch of the logic in this section
  ;; is stolen from org-capture.el
  ;; so thank you to all of the engineers
  ;; who have worked on org over the years
  ;; for making my job so much easier.

  (defvar ch/notion/plist '())

  (defun ch/notion/plist/put (&rest keypairs)
    (let ((keypairs (-partition 2 keypairs)))
      (dolist (pair keypairs)
	(pcase pair
	  (`(,key ,value) (setq ch/notion/plist
				(plist-put ch/notion/plist key value)))))
      ch/notion/plist))

  (defun ch/notion/plist/get (key)
    (plist-get ch/notion/plist key))

  (defvar ch/notion/capture-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "C-c C-c") #'ch/notion/finalize)
      (define-key map (kbd "C-c C-k") #'ch/notion/kill)
      map))

  (define-minor-mode ch/notion/capture-mode
    ""
    :keymap ch/notion/capture-mode-map
    (setq-local
     header-line-format
     (substitute-command-keys
      "\\<ch/notion/capture-mode-map>Capture buffer. Finish \
`\\[ch/notion/finalize]', abort `\\[ch/notion/kill]'.")))

  (defun ch/notion/finalize ()
    ;; TODO: better error handling
    (interactive)
    (let* ((capture-buffer (ch/notion/plist/get :capture-buffer))
	   (return-winconf (ch/notion/plist/get :return-winconf))
	   (results (ch/notion/plist/get :results))
	   (body (with-current-buffer capture-buffer (buffer-string))))
      (ht-set results "body" body)
      (make-thread
       (lambda ()
	 (condition-case err
	     (ch/notion/proc/join (ch/notion/proc/capture results))
	   (t (thread-signal main-thread (car err) (cdr err))))))
      (setq ch/notion/plist '())
      (set-window-configuration return-winconf)
      (kill-buffer capture-buffer)))

  (defun ch/notion/kill ()
    ;; TODO: better error handling
    (interactive)
    (let ((capture-buffer (ch/notion/plist/get :capture-buffer))
	  (return-winconf (ch/notion/plist/get :return-winconf)))
      (setq ch/notion/plist '())
      (set-window-configuration return-winconf)
      (kill-buffer capture-buffer)))

  (defun ch/notion/proc/dump ()
    (make-process
     :name "notion-cli-dump"
     :buffer (generate-new-buffer "notion-cli-dump")
     :command '("notion-cli" "dump")
     :sentinel #'ignore))

  (defun ch/notion/proc/capture (args)
    (make-process
     :name "notion-cli-capture"
     :buffer "notion-cli-capture"
     :command `("notion-cli" "capture" "-interactive=false" ,(format "-propinfo=%s" (json-encode-hash-table args)))
     :sentinel #'ignore))

  (defun ch/notion/proc/join (proc)
    (while (accept-process-output proc))
    (let* ((exit-status (process-exit-status proc))
	   (buffer (process-buffer proc))
	   (contents
	    (with-current-buffer buffer
	      (buffer-string))))
      (kill-buffer buffer)
      (if (not (= 0 exit-status))
	  (error "process '%s' exited with nonzero exit code:\n%s" proc contents)
	contents)))

  (defun ch/notion/prompt/select (name options)
    (ivy-read (concat name ": ")
	      options
	      :require-match t))

  (defun ch/notion/prompt (prop-name prop)
    (if (string= (ht-get prop "type") "select")
	(ch/notion/prompt/select prop-name (ch/vec-to-list (ht-get prop "options")))
      (read-string (concat prop-name ": "))))

  (defun ch/notion/required-props (config)
    (let ((props (gethash "properties" config)))
      (-filter (lambda (prop-name)
		 (not (gethash "default" (gethash prop-name props))))
	       (hash-table-keys props))))

  (defun ch/notion/prompt-order (config)
    (let* ((required-props (ch/notion/required-props config))
	   (order (-filter (lambda (prop) (-contains? required-props prop))
			   (ch/vec-to-list (gethash "order" config))))
	   (remaining-props (-filter
			     (lambda (prop) (not (-contains? order prop)))
			     required-props)))
      (append order remaining-props)))

  (defun ch/notion/capture ()
    (interactive)
    (let* ((proc (ch/notion/proc/dump))

	   (title (condition-case ()
		      (read-string "Task: ")
		    (quit (progn
			    (ch/notion/proc/join proc)
			    (keyboard-quit)))))

	   (config (json-parse-string (ch/notion/proc/join proc)))
	   (prompt-order (ch/notion/prompt-order config))

	   (title-name nil)
	   (results (ht)))

      (dolist (prop-name prompt-order)
	(let ((prop (ht-get* config "properties" prop-name)))
	  (if (string= (ht-get prop "type") "title")
	      (progn
		(setq title-name prop-name)
		(ht-set results prop-name title))
	    (ht-set results prop-name (ch/notion/prompt prop-name prop)))))

      (let ((capture-buffer (generate-new-buffer "notion-capture")))
       (ch/notion/plist/put :return-winconf (current-window-configuration)
			    :capture-buffer capture-buffer
			    :results (ht ("properties" results)
					 ("title" title-name)))

       (delete-other-windows)
       (switch-to-buffer capture-buffer)
       (markdown-mode)
       (ch/notion/capture-mode)))))
