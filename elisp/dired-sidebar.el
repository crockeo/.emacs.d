(require 'dired)

;; Adding a global hotkey to toggle the dired-sidebar.
(global-set-key (kbd "C-c d") 'dired-sidebar-toggle-sidebar)

;; Toggling directories in dired sidebar
(bind-key (kbd "TAB") 'dired-subtree-toggle dired-mode-map)
