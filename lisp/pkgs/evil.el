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

  (defun ch/evil/kill-buffers ()
    (interactive)
    (when (yes-or-no-p "Close all buffers? ")
      (switch-to-buffer "*scratch*")
      (mapc #'kill-buffer
	    (ch/evil/kill-buffers/filter (buffer-list)))))

  (defun ch/evil/copy-file-link ()
    (interactive)
    (let ((file-link (concat "file:"
			     (file-truename buffer-file-name)
			     "::"
			     (number-to-string (line-number-at-pos)))))
      (kill-new file-link)
      (message "copied link: %s" file-link)))

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
      (kbd "C-SPC") 'company-complete)

    (evil-define-key nil evil-normal-state-map
      ";" 'ch/evil/last-file-buffer

      (kbd "C-c c") 'evilnc-comment-or-uncomment-lines

      (kbd "C-c f l") 'ch/evil/copy-file-link

      (kbd "C-c j b") 'xref-pop-marker-stack
      (kbd "C-c j g") 'lsp-find-definition
      (kbd "C-c j i") 'lsp-find-implementation
      (kbd "C-c j r") 'lsp-find-references

      (kbd "C-c n o") 'org-capture
      (kbd "C-c n r") 'org-roam-capture

      (kbd "C-c p a") 'counsel-projectile-ag
      (kbd "C-c p f") 'counsel-projectile-find-file
      ;; TODO: come back and use counsel
      ;; after i figure out performance issues
      (kbd "C-c p p") 'projectile-switch-project
      (kbd "C-c p r") 'projectile-discover-projects-in-search-path

      (kbd "C-c k") 'ch/evil/kill-buffers

      (kbd "C-c s") 'lsp-format-buffer

      "u" 'undo-fu-only-undo
      (kbd "C-r") 'undo-fu-only-redo

      (kbd "C-s C-n") 'ch/evil/vterm
      (kbd "C-s o") 'other-window
      (kbd "C-s C-o") 'other-window
      (kbd "C-s %") 'split-window-right
      (kbd "C-s \"") 'split-window-below)

    (when (require 'org)
      (evil-define-key 'normal org-mode-map
	(kbd "C-c o a") 'ch/org/archive
	(kbd "C-c o c") 'ch/org/complete
	(kbd "C-c o i") 'org-roam-node-insert
	(kbd "C-c o r") 'ch/org/refile
	(kbd "C-c o s") 'ch/org/todo-sort)))

  (use-package evil-nerd-commenter))
