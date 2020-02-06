;; Install packages
(use-package lean-mode
  :ensure t)

(use-package company-lean
  :after company lean-mode
  :ensure t)

(require 'company)

;; Opening the lean error and goal windows when we open a lean file.
(defun open-lean-windows ()
  (defvar original-buffer)
  (setq original-buffer (current-buffer))

  ;; Making sure we don't create a bunch new windows.
  (delete-other-windows)

  ;; Setting up our windows with those buffers live
  (select-window (split-window-right 120))
  (switch-to-buffer "*Lean Goal*")
  (select-window (split-window-below))
  (switch-to-buffer "*Lean Next Error*")

  ;; Coming back home <3
  (other-window 1)
  (switch-to-buffer original-buffer))

;; Setting up Lean
(defun setup-lean ()
  ;; Opening error and goal windows by default
  (open-lean-windows)

  ;; Adding lean to the list of company backends
  (add-to-list 'company-backends 'company-lean))

(add-hook 'lean-mode-hook 'setup-lean)