;;; hawaii-theme.el --- hawaii theme dot ee el
;;;
;;; Version: 1.0
;;;
;;; Commentary:
;;;   hawaii-theme.el is based off of the color scheme of these keycaps:
;;;
;;;     https://mechanicalkeyboards.com/shop/index.php?l=product_detail&p=2352
;;;
;;;   Yes, I just wanted to have my emacs theme match me keyboard.
;;;
;;; Code:

(defconst hawaii-background "#332a24")
(defconst hawaii-background-dark "#40352d")
(defconst hawaii-comment "#80695b")
(defconst hawaii-error "#ff0000")
(defconst hawaii-highlight-blue "#79eaf2")
(defconst hawaii-highlight-green "#aff279")
(defconst hawaii-highlight-orange "#ffb080")
(defconst hawaii-highlight-purple "#bb80ff")
(defconst hawaii-highlight-red "#ff4040")
(defconst hawaii-text "#f2f2f2")

(deftheme hawaii "DOCSTRING for hawaii")

(custom-theme-set-faces
 'hawaii

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; Built-In Configuration ;;

 `(cursor
   ((t (:background ,hawaii-highlight-orange))))

 `(default
    ((t (:foreground ,hawaii-text :background ,hawaii-background))))

 `(font-lock-builtin-face
   ((t (:foreground ,hawaii-highlight-green :bold t))))

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

 `(minibuffer-prompt
   ((t (:foreground ,hawaii-text :bold t))))

 `(mode-line
   ((t (:foreground ,hawaii-text :background ,hawaii-background-dark))))

 `(region
   ((t (:foreground ,hawaii-background :background ,hawaii-highlight-orange)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'hawaii)

;;; hawaii-theme.el ends here
