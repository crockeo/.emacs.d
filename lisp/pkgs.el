;;; pkgs.el -*- lexical-binding: t; -*-


;; TODO: move these their own files under ~/.emacs.d/lisp/pkgs
;; and then require, e.g. ch/pkg/evil
(ch/pkg evil ()
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
  
  (use-package undo-fu)
  
  (use-package evil
    :init (evil-mode 1)
    :config
    (ch/evil/define-key-all (evil-insert-state-map evil-normal-state-map evil-visual-state-map)
      "\C-a" 'ch/evil/start-of-line
      "\C-e" 'ch/evil/end-of-line)

    (evil-define-key nil evil-insert-state-map
      "\C-f" 'evil-normal-state)
    
    (evil-define-key nil evil-normal-state-map
      "u" 'undo-fu-only-undo
      (kbd "C-c c") 'evilnc-comment-or-uncomment-lines
      "\C-r" 'undo-fu-only-redo))

  (use-package evil-nerd-commenter))


(ch/pkg ivy ()
  (use-package ivy
    :init (ivy-mode 1))

  (use-package prescient)

  (use-package ivy-prescient
    :after (ivy prescient)
    :hook (ivy-mode . ivy-prescient-mode)))


(ch/pkg lisp (emacs-lisp-mode-hook)
  (use-package eros
    :init (eros-mode 1)
    :hook (emacs-lisp-mode . eros-mode))

  (use-package paredit
    :init (paredit-mode 1)
    :hook (emacs-lisp-mode . paredit-mode)))

(ch/use-pkgs
  evil
  ivy
  lisp)
