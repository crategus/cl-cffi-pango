(in-package :pango-example)

(defun pango-draw-text-centered (cr width height)
  (let ((layout (pango:cairo-create-layout cr)))
    (cairo:save cr)
    ;; Clear the background
    (cairo:set-source-rgb cr 1.0 1.0 1.0)
    (cairo:paint cr)
    ;; Set the color
    (cairo:set-source-rgb cr 0.5 0.5 0.5)
    ;; Set the font
    (setf (pango:layout-font-description layout)
          (pango:font-description-from-string "Courier Bold 60"))
    ;; Set the text
    (setf (pango:layout-text layout) "Crategus")
    ;; Extents of the layout in pixel Units
    (multiple-value-bind (text-width text-height)
        (pango:layout-pixel-size layout)
      ;; Move to the start position of the text
      (cairo:move-to cr (- (/ width 2) (/ text-width 2))
                        (- (/ height 2) (/ text-height 2)))
      ;; Print the text on the Cario context
      (pango:cairo-show-layout cr layout)
      (cairo:restore cr))))

;;; --- 2023-2-12 --------------------------------------------------------------
