;; Opening the lean error and goal windows when we open a lean file.
(defun open-lean-windows ()
  (defvar original-buffer)
  (setq original-buffer (current-buffer))
  
  ;; Setting up our windows with those buffers live
  (select-window (split-window-right 120))
  (switch-to-buffer "*Lean Goal*")
  (select-window (split-window-below))
  (switch-to-buffer "*Lean Next Error*")

  ;; Coming back home <3
  (other-window 1)
  (switch-to-buffer original-buffer))

(add-hook 'lean-mode-hook 'open-lean-windows)
