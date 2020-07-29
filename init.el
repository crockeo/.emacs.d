;; HEY, here's a reminder. Is Emacs running slowly? Do you want to fix that?
;;
;; * Start a profiler: M-x profiler-start
;; * See the results:  M-x profiler-results

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting up Package Management ;;
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq use-package-always-ensure t)

(defun is-emacs-27 ()
  (version< "27.0" emacs-version))

;;;;;;;;;;;;;;;;;;
;; Package List ;;
(add-to-list 'load-path "~/.emacs.d/elisp")

(use-package quelpa)
(require 'git-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading Configurations ;;

;; Packges that only need a load-package, but nothing beyond that.
(load-library "loner-conf")

;; Package configurations
(load-library "company-conf")
(load-library "dired-sidebar-conf")
(load-library "display-line-number-conf")
(load-library "doom-modeline-conf")
(load-library "evil-conf")
(load-library "flycheck-conf")
(load-library "git-gutter-fringe-conf")
(load-library "git-link-conf")
(load-library "helm-conf")
(load-library "lsp-conf")
(load-library "paredit-conf")
(load-library "projectile-conf")
(load-library "slime-conf")
(load-library "smart-jump-conf")

;; Language configuration
(load-library "clojure-conf")
(load-library "fennel-conf")
(load-library "glsl-conf")
(load-library "go-conf")
(load-library "lean-conf")
(load-library "salt-conf")

(unless (is-emacs-27)
  (load-library "lua-conf"))

(load-library "protobuf-conf")
(load-library "python-conf")
(load-library "markdown-conf")
(load-library "yaml-conf")

;; Custom packages / configurations / etc.
(load-library "format")

(load-library "highlight-todo")
(global-highlight-todo-mode 1)

(load-library "crockeo-mode")
(global-crockeo-mode 1)

;; Builtin configuration
(load-library "builtin-conf")

;; TODO: Move this elsewhere
(setq org-agenda-files '("~/home.org"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom Configurations ;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9" default))
 '(org-agenda-files '("~/home.org"))
 '(package-selected-packages
   '(quelpa lsp-pyright yasnippet paredit slime tree-sitter git-link magit python-docstring cython-mode company-box exec-path-from-shell salt-mode yaml-mode yaml ag srcery-theme editorconfig protobuf-mode lsp-ui helm-lsp lsp-mode smart-jump floobits glsl-mode fennel fennel-mode lua-mode company-jedi jedi eros dumb-jump fill-column-indicator cider company-go go-mode jedi-company company-quickhelp dired-sidebar company-lean company flychecker projectile evil-nerd-commenter lean-mode markdown-mode use-package helm evil-visual-mark-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
