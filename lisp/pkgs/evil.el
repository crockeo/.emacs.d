;;; evil.el -*- lexical-binding: t; -*-

(ch/pkg evil
  (defmacro ch/evil/define-key-all (states &rest body)
    (declare (indent defun))
    `(progn
       ,@(mapcar
	  (lambda (state)
	    `(evil-define-key nil ,state
	       ,@body))
	  states)))

  (defun ch/evil/start-of-line ()
    (interactive)
    (evil-first-non-blank))

  (defun ch/evil/end-of-line ()
    (interactive)
    (evil-end-of-line)
    (unless (or (eq evil-state 'visual)
		(equal "" (buffer-substring (line-beginning-position) (line-end-position))))
      (forward-char)))

  (defun ch/evil/last-file-buffer ()
    (interactive)
    (let* ((file-buffers (seq-filter #'buffer-file-name (buffer-list)))
	   (last-file-buffer (when (> (length file-buffers) 1)
			       (cadr file-buffers))))
      (switch-to-buffer last-file-buffer)))

  (defun ch/evil/kill-buffers/filter (buffer-list)
    (let ((current-buffer (current-buffer)))
      (seq-filter (lambda (buffer)
		    (not (or (eq buffer current-buffer)
			     (company-box--is-box-buffer buffer))))
		  buffer-list)))

  (defun ch/evil/kill-buffer (buffer)
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
	(let ((server (eglot-current-server)))
	  (when server
	    (eglot-shutdown server))))
      (kill-buffer buffer)))

  (defun ch/evil/kill-buffers ()
    (interactive)
    (when (yes-or-no-p "Close all buffers? ")
      (switch-to-buffer "*scratch*")
      (mapc #'ch/evil/kill-buffer
	    (ch/evil/kill-buffers/filter (buffer-list)))))

  (defun ch/evil/copy-file-link ()
    (interactive)
    (let ((file-link (concat "file:"
			     (file-truename buffer-file-name)
			     "::"
			     (number-to-string (line-number-at-pos)))))
      (kill-new file-link)
      (message "copied link: %s" file-link)))

  (defun ch/evil/complete ()
    (interactive)
    (if (copilot-current-completion)
	(copilot-accept-completion)
      (company-complete)))

  (use-package undo-fu)

  ;; TODO: move global hotkeys over to crockeo.el instead
  (use-package evil
    :init (evil-mode 1)
    :config
    (evil-set-initial-state 'xref--xref-buffer-mode 'emacs)

    (ch/evil/define-key-all (evil-insert-state-map evil-normal-state-map evil-visual-state-map)
      "\C-a" 'ch/evil/start-of-line
      "\C-e" 'ch/evil/end-of-line)

    (evil-define-key nil evil-insert-state-map
      "\C-f" 'evil-normal-state
      (kbd "C-SPC") 'ch/evil/complete)

    (evil-define-key nil evil-normal-state-map
      ";" 'ch/evil/last-file-buffer

      (kbd "C-c 1") 'profiler-start
      (kbd "C-c 2") 'profiler-stop

      (kbd "C-c c") 'evilnc-comment-or-uncomment-lines

      (kbd "C-c f l") 'ch/evil/copy-file-link

      (kbd "C-c g c") 'magit-clone
      (kbd "C-c g l") 'lgc  ;; spooky, what does it mean!?

      (kbd "C-c i d") 'ch/org/download-files
      (kbd "C-c i s") 'ch/org/sync-files
      (kbd "C-c i u") 'ch/org/upload-files

      (kbd "C-c j b") 'xref-go-back
      (kbd "C-c j d") 'eglot-code-actions
      (kbd "C-c j g") 'xref-find-definitions
      (kbd "C-c j i") 'eglot-find-implementation
      (kbd "C-c j q") 'eglot-rename
      (kbd "C-c j r") 'xref-find-references

      (kbd "C-c n d") 'org-roam-dailies-capture-today
      (kbd "C-c n t") 'org-roam-dailies-capture-tomorrow

      (kbd "C-c l l") 'ch/olivetti/toggle-ensure
      (kbd "C-c l p") 'ch/builtin/toggle-present

      (kbd "C-c p a") 'counsel-projectile-ag
      (kbd "C-c p f") 'counsel-projectile-find-file
      ;; TODO: come back and use counsel
      ;; after i figure out performance issues
      (kbd "C-c p p") 'projectile-switch-project
      (kbd "C-c p r") 'projectile-discover-projects-in-search-path
      (kbd "C-c p s") 'projectile-replace-regexp

      (kbd "C-c k") 'ch/evil/kill-buffers

      (kbd "C-c s") 'eglot-format-buffer

      (kbd "C-c t j") 'json-pretty-print

      "u" 'undo-fu-only-undo
      (kbd "C-r") 'undo-fu-only-redo

      (kbd "C-s C-n") 'ch/evil/vterm
      (kbd "C-s o") 'other-window
      (kbd "C-s C-o") 'other-window
      (kbd "C-s %") 'split-window-right
      (kbd "C-s \"") 'split-window-below))

  (use-package evil-nerd-commenter))
