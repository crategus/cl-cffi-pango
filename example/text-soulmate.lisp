(in-package :pango-example)

(defparameter text
"Most relationships seem so transitory
They're all good but not the permanent one
Who doesn't long for someone to hold
Who knows how to love you without being told
Somebody tell me why I'm on my own
If there's a soulmate for everyone")

(defun pango-draw-text-soulmate (cr width height)
  (let ((layout (pango:cairo-create-layout cr)))
    (cairo:save cr)
    ;; Clear the background
    (cairo:set-source-rgb cr 1.0 1.0 1.0)
    (cairo:paint cr)
    ;; Set the color.
    (cairo:set-source-rgb cr 0.1 0.1 0.1)
    ;; Select the font face
    (cairo:select-font-face cr "Purisa" :weight :bold)
    ;; Specify the font size
    (cairo:set-font-size cr 13)
    ;; Set the font
    (setf (pango:layout-font-description layout)
          (pango:font-description-from-string "Purisa Normal 12"))
    ;; Set more line spacing
    (setf (pango:layout-line-spacing layout) 1.5)
    ;; Set the text
    (setf (pango:layout-text layout) text)
    ;; Extents of the layout in pixel Units
    (multiple-value-bind (text-width text-height)
        (pango:layout-pixel-size layout)
      ;; Move to the start position of the text
      (cairo:move-to cr (- (/ width 2) (/ text-width 2))
                        (- (/ height 2) (/ text-height 2)))
      ;; Print the text on the Cario context
      (pango:cairo-show-layout cr layout))
    (cairo:restore cr)))

;;; --- 2023-2-12 --------------------------------------------------------------
