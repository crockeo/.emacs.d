;; Constructs the numeric RGB representation of a color from the provided color.
(defmacro color-component (name start end)
  `(defun ,name (color)
     (/ (string-to-number (substring color ,start ,end) 16) 255.00)))

(color-component r 1 3)
(color-component g 3 5)
(color-component b 5 7)

(defvar iterm-generator--color-template
  "    <key>%s</key>
    <dict>
        <key>Alpha Component</key>
        <real>1</real>
        <key>Blue Component</key>
        <real>%f</real>
        <key>Color Space</key>
        <string>sRGB</string>
        <key>Green Component</key>
        <real>%f</real>
        <key>Red Component</key>
        <real>%f</real>
    </dict>")

(defvar iterm-generator--theme-template
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
%s
</dict>
</plist>")

(defvar iterm-generator--color-name-lookup
  '((foreground . "Foreground Color")
    (background . "Background Color")
    (bold . "Bold Color")

    (links . "Link Color")

    (selection . "Selection Color")
    (selected-text . "Selected Text Color")

    (cursor . "Cursor Color")
    (cursor-text . "Cursor Text Color")

    (black . "Ansi 0 Color")
    (red . "Ansi 1 Color")
    (green . "Ansi 2 Color")
    (yellow . "Ansi 3 Color")
    (blue . "Ansi 4 Color")
    (magenta . "Ansi 5 Color")
    (cyan . "Ansi 6 Color")
    (white . "Ansi 7 Color")

    (black-bright . "Ansi 8 Color")
    (red-bright . "Ansi 9 Color")
    (green-bright . "Ansi 10 Color")
    (yellow-bright . "Ansi 11 Color")
    (blue-bright . "Ansi 12 Color")
    (magenta-bright . "Ansi 13 Color")
    (cyan-bright . "Ansi 14 Color")
    (white-bright . "Ansi 15 Color")))

(defun iterm-generator--generate-color (color-name color)
  (format iterm-generator--color-template
          color-name
          (b color)
          (g color)
          (r color)))

(defun iterm-generator--generate-theme (color-scheme)
  (format iterm-generator--theme-template
          (mapconcat (lambda (color-pair)
                       (let* ((internal-color-name (nth 0 color-pair))
                              (color (nth 1 color-pair))
                              (color-name (cdr (assoc internal-color-name iterm-generator--color-name-lookup))))
                         (iterm-generator--generate-color color-name color)))
                     color-scheme
                     "\n")))

(defun iterm-generator--generate-theme-to-file (file-name color-scheme)
  (with-temp-file file-name
    (insert (iterm-generator--generate-theme color-scheme))))

(defun generate-hawaii-theme-iterm (file-name)
  "Generates the hawaii color scheme as a theme for iTerm2"

  (require 'hawaii-theme)

  (iterm-generator--generate-theme-to-file
   file-name
   `(;; Text
     (foreground ,hawaii-text)
     (background ,hawaii-background)
     (bold ,hawaii-text)

     (links ,hawaii-highlight-blue)

     (selection ,hawaii-highlight-orange)
     (selected-text ,hawaii-background)

     (cursor ,hawaii-highlight-orange)
     (cursor-text ,hawaii-background)

     ;; ANSI colors
     (black ,hawaii-background)
     (red ,hawaii-highlight-red)
     (green ,hawaii-highlight-green)
     (yellow ,hawaii-highlight-orange)
     (blue ,hawaii-highlight-blue)
     (magenta ,hawaii-highlight-purple)
     (cyan ,hawaii-highlight-blue)
     (white ,hawaii-text)

     (black-bright ,hawaii-background)
     (red-bright ,hawaii-highlight-red)
     (green-bright ,hawaii-highlight-green)
     (yellow-bright ,hawaii-highlight-orange)
     (blue-bright ,hawaii-highlight-blue)
     (magenta-bright ,hawaii-highlight-purple)
     (cyan-bright ,hawaii-highlight-blue)
     (white-bright ,hawaii-text))))

(generate-hawaii-theme-iterm "~/hawaii.itermcolors")

;; (generate-hawaii-theme-iterm "hawaii-iterm.xml")
