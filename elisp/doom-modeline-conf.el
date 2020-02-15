;; Installing the package
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

(require 'doom-modeline)

;; Setting up doom-modline to play well with projectile
(setq doom-modeline-project-detection 'projectile)
(setq doom-modeline-buffer-file-name-style 'relative-from-project)

;; Setting up icon configuration
(setq doom-modeline-icon (display-graphic-p))
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-buffer-state-icon nil)

;; Setting up version control
(setq doom-modeline-vcs-max-length 12)

;; Constructing a doom-modeline segment that represents the window
;; configurations provided by winpop.
(load-library "winpop")

(doom-modeline-def-segment crockeo-modeline-winpop
  "Provides the list of window configurations that are currently on the stack."
  (concat
   " ("
   (string-join (winpop-get-names)
                " > ")
   ") "))

;; Configuring the elements on the modeline
(doom-modeline-def-modeline 'crockeo-modeline
  '(bar
    modals
    buffer-info
    remote-host
    buffer-position
    crockeo-modeline-winpop)

  '(major-mode
    vcs
    checker))

(defun setup-crockeo-modeline ()
  (doom-modeline-set-modeline 'crockeo-modeline 'default))

(add-hook 'doom-modeline-mode-hook 'setup-crockeo-modeline)
