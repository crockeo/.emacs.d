(defconst hawaii-background "#332a24")
(defconst hawaii-background-light "#40352d")
(defconst hawaii-background-dark "#26201b")
(defconst hawaii-comment "#997e6d")
(defconst hawaii-error "#ff0000")
(defconst hawaii-highlight-blue "#79eaf2")
(defconst hawaii-highlight-green "#aff279")
(defconst hawaii-highlight-orange "#ffb080")
(defconst hawaii-highlight-purple "#bb80ff")
(defconst hawaii-highlight-red "#ff4040")
(defconst hawaii-text "#ffffff")

(deftheme hawaii "DOCSTRING for hawaii")

(custom-theme-set-faces
 'hawaii

 ;;;;;;;;;;;;;;
 ;; Built-In ;;

 `(cursor
   ((t (:background ,hawaii-highlight-orange))))

 `(default
    ((t (:foreground ,hawaii-text :background ,hawaii-background))))

 `(font-lock-builtin-face
   ((t (:foreground ,hawaii-highlight-purple :bold t))))

 `(font-lock-comment-face
   ((t (:foreground ,hawaii-comment))))

 `(font-lock-constant-face
   ((t (:foreground ,hawaii-highlight-purple))))

 `(font-lock-function-name-face
   ((t (:foreground ,hawaii-highlight-green))))

 `(font-lock-keyword-face
   ((t (:foreground ,hawaii-highlight-blue))))

 `(font-lock-string-face
   ((t (:foreground ,hawaii-highlight-orange))))

 `(font-lock-type-face
   ((t (:foreground ,hawaii-highlight-blue))))

 `(font-lock-variable-name-face
   ((t (:foreground ,hawaii-highlight-green))))

 `(font-lock-warning-face
   ((t (:foreground ,hawaii-highlight-red))))

 `(fringe
   ((t (:background ,hawaii-background))))

 `(link
   ((t (:foreground ,hawaii-highlight-blue :underline t))))

 `(link-visited
   ((t (:foreground ,hawaii-highlight-purple :underline t))))

 `(minibuffer-prompt
   ((t (:foreground ,hawaii-text :bold t))))

 `(mode-line
   ((t (:foreground ,hawaii-text :background ,hawaii-background-light))))

 `(region
   ((t (:foreground ,hawaii-background :background ,hawaii-highlight-orange))))

 `(success
   ((t (:foreground ,hawaii-highlight-green))))

 ;;;;;;;;;;;;;
 ;; Company ;;

 `(company-preview-common
   ((t (:foreground ,hawaii-comment :background ,hawaii-background))))

 `(company-scrollbar-bg
   ((t (:background ,hawaii-background-light :box ,hawaii-background-dark))))

 `(company-scrollbar-fg
   ((t (:background ,hawaii-text))))

 `(company-tooltip
   ((t (:foreground ,hawaii-text :background ,hawaii-background-light))))

 `(company-tooltip-common
   ((t (:foreground ,hawaii-highlight-green))))

 `(company-tooltip-selection
   ((t (:foreground ,hawaii-background :background ,hawaii-highlight-orange))))

 `(company-tooltip-annotation
   ((t (:foreground ,hawaii-highlight-blue))))

 `(company-tooltip-annotation-selection
   ((t (:foreground ,hawaii-comment))))

 `(company-posframe-quickhelp
   ((t (:foreground ,hawaii-text :background ,hawaii-background-light))))

 `(company-posframe-quickhelp-header
   ((t (:foreground ,hawaii-text :background ,hawaii-background-light :extend t :box ,hawaii-background-dark))))

 ;;; hl-diff

 `(diff-hl-insert
   ((t (:foreground ,hawaii-highlight-green))))

 `(diff-hl-delete
   ((t (:foreground ,hawaii-highlight-red))))

 `(diff-hl-change
   ((t (:foreground ,hawaii-highlight-orange))))

 ;;;;;;;;;;
 ;; Helm ;;

 `(helm-selection
   ((t (:foreground ,hawaii-background :background ,hawaii-highlight-orange))))

 ;;;;;;;;;;;;;;;;;
 ;; Line Number ;;

 `(line-number
   ((t (:foreground ,hawaii-comment :background ,hawaii-background))))

 ;;;;;;;;;;;;;;;
 ;; Mode Line ;;

 `(mode-line
   ((t (:foreground ,hawaii-text :background ,hawaii-background-light))))

 `(mode-line-emphasis
   ((t (:foreground ,hawaii-highlight-blue))))

 `(mode-line-highlight
   ((t (:foreground ,hawaii-highlight-purple))))

 `(mode-line-inactive
   ((t (:foreground ,hawaii-comment :background ,hawaii-background-light))))

 ;;;;;;;;;
 ;; Org ;;

 `(org-level-1
   ((t (:foreground ,hawaii-highlight-green :height 1.3))))

 `(org-level-2
   ((t (:foreground ,hawaii-highlight-blue :height 1.2))))

 `(org-level-3
   ((t (:foreground ,hawaii-highlight-orange :height 1.1))))

 `(org-level-4
   ((t (:foreground ,hawaii-highlight-purple)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'hawaii-theme)
(provide-theme 'hawaii)
