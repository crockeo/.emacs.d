(require 'jedi-core)
(require 'posframe)

(defvar-local jedi-posframe-buffer-name "jedi-posframe")
(defvar-local jedi-posframe-last-pos nil)
(defvar-local jedi-posframe-styles
  '(;; Styling the function call
    ((rx (seq (group (+? anything)) "("))
     1
     'font-lock-function-name-face)

    ;; Styling variables
    ((rx (seq
          "    "
          (group (+? anything))
          (or ": "
              (seq (*? (in space))
                   "="
                   (*? (in space))))))
     1
     'font-lock-variable-name-face)

    ;; Styling types
    ((rx (seq
          ":"
          (? " ")
          (group (+? anything))
          " = "))
     1
     'font-lock-type-face)

    ;; Styling built-ins
    ((rx (group
          (or "True"
              "False"
              "None")))
     1
     'font-lock-builtin-face)))

(defun jedi-posframe-style-buffer (buffer)
  "Applies styling to a buffer, which contains the contents of the Jedi tooltip"
  (with-current-buffer buffer
    (dolist (style jedi-posframe-styles)
      (let ((regex (nth 0 style))
            (group-num (nth 1 style))
            (font-face (nth 2 style)))
        (progn
          (goto-char (point-min))

          (while (re-search-forward (eval regex) nil t)
            (add-face-text-property
             (match-beginning group-num)
             (match-end group-num)
             font-face)))))))

(defun jedi-posframe-show-tooltip (contents)
  "Constructs and shows a posframe with the requested contents."
  (when (posframe-workable-p)
    (let ((target-buffer (get-buffer-create jedi-posframe-buffer-name)))
      (progn
        (posframe-show
         (get-buffer-create jedi-posframe-buffer-name)
         :internal-border-color "#75715E"
         :internal-border-width 1
         :position (point)
         :string contents
         :left-margin-width 1)
        (jedi-posframe-style-buffer target-buffer)))))

(defun jedi-posframe-hide-tooltip ()
  "Hides the posfram created by jedi-posframe-show-tooltip."
  (when (bound-and-true-p jedi-posframe-mode)
    (posframe-hide jedi-posframe-buffer-name)))

(defun jedi-posframe-maybe-hide-tooltip ()
  "Hides the posframe created by jedi-posframe-show-tooltip, but only when the
cursor has moved."
  (let ((cur-pos (point)))
    (when (and (not (eq jedi-posframe-last-pos cur-pos))
               (bound-and-true-p jedi-posframe-mode))
      (setq jedi-posframe-last-pos cur-pos)
      (jedi-posframe-hide-tooltip))))

(defun jedi-posframe-format-contents (contents)
  "Formats the contents to be split across multiple lines. For example, if the
input were:

User(username: string, password: string)

This function would return

User(
  username: string,
  password: string,
)"
  (replace-regexp-in-string
   (regexp-quote ")")
   ",\n)"
   (replace-regexp-in-string
    "\\((\\|, \\)"
    "\\1\n    "
    contents
    nil)
   nil))

(defun jedi-posframe (oldfunc contents)
  "Preprocesses the contents requested to be shown, and then shows them usig
`jedi-posframe-show-tooltip'."
  (jedi-posframe-show-tooltip
    (jedi-posframe-format-contents contents)))

(define-minor-mode jedi-posframe-mode
  "A mode for moving jedi function tooltips to posframe. Implemented as a
minor-mode to maintain hook state."
  :lighter nil
  (if jedi-posframe-mode
      ;; Setting up jedi-posframe-mode
      (progn
        (advice-add
         'jedi:tooltip-show
         :around #'jedi-posframe)

        (add-hook 'window-configuration-change-hook
                  'jedi-posframe-hide-tooltip)

        (add-hook 'post-command-hook
                  'jedi-posframe-maybe-hide-tooltip))

    ;; Tearing down jedi-posframe-mode
    (progn
      (posframe-delete jedi-posframe-buffer-name)

      (advice-remove
       'jedi:tooltip-show
       #'jedi-posframe)

      (remove-hook 'window-configuration-change-hook
                   'jedi-posframe-hide-toolip)

      (remove-hook 'post-command-hook
                   'jedi-posframe-maybe-hide-tooltip))))

(provide 'jedi-posframe-mode)
