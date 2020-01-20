;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Setting up Package Management ;
(require 'package)
 
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;
; Package List ;
(use-package evil :ensure t) ; Provides a vim-like interface
(use-package helm :ensure t) ; Provides Ctrl-P like interface
(use-package monokai-theme :ensure t) ; Provides the monokai theme (from TextMate)

; Language-specific packages
(use-package markdown-mode :ensure t) ; Markdown
; TODO: Find and install language-specific packages

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Loading Configurations ;
(add-to-list 'load-path "~/.emacs.d/elisp/")

(load-library "hotkeys") ; Hotkeys used across emacs
(load-library "hotkeys-evil-mode") ; Hotkeys specifically for evil-mode

;;;;;;;;;;;;;;;;;;;;;;;
; Misc Configurations ;

; Ensuring that evil-mode is used on startup
(require 'evil)
(evil-mode)

; Moving all backups to a single directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

; Removing emacs menu bar / tool bar
(tool-bar-mode -1)
(menu-bar-mode -1)

; Adding line numbers to the left
(global-display-line-numbers-mode)

; TODO: Fix new line causing comments to jump right; ask Nathan
;       Space for fix

;;;;;;;;;;;;;;;;;;;;;;;;;
; Custom Configurations ;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (markdown-mode use-package helm evil-visual-mark-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
