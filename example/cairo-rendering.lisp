(in-package :pango-example)

(defun pango-draw-cairo-rendering (cr width height)
  (let ((radius (- (/ (min width height) 2) 20))
        (circle 260)
        (n-words 12)
        (font "Sans Bold 16"))
    ;; Set up a transformation matrix so that the user space
    ;; coordinates for where we are drawing are [-RADIUS, RADIUS],
    ;; [-RADIUS, RADIUS] We first center, then change the scale
    (cairo:translate cr (+ radius (/ (- width (* 2 radius)) 2))
                        (+ radius (/ (- height (* 2 radius)) 2)))
    (cairo:scale cr (/ radius circle) (/ radius circle))
    ;; Clear surface
    (cairo:set-source-rgb cr 1.0 1.0 1.0)
    (cairo:paint cr)
    ;; Create a PangoLayout, set the font and text
    (let ((layout (pango:cairo-create-layout cr))
          (desc (pango:font-description-from-string font)))
      (setf (pango:layout-text layout) "Crategus")
      (setf (pango:layout-font-description layout) desc)
      ;; Draw the layout n-words times in a circle
      (do* ((i 0 (+ i 1))
            (angle 0 (/ (* 360 i) n-words))
            ;; Gradient color
            (color (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)
                   (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)))
           ((>= i n-words))
           (cairo:save cr)
           (cairo:set-source-rgb cr (/ #xFF 255) (/ #x99 255) color)
           (cairo:rotate cr (/ (* angle pi) 180))
           ;; Inform Pango to re-layout the text with the new
           ;; transformation matrix
           (pango:cairo-update-layout cr layout)
           (multiple-value-bind (width height)
               (pango:layout-size layout)
             (declare (ignore height))
             (cairo:move-to cr (- (/ width 2 pango:+scale+)) (- circle)))
             (pango:cairo-show-layout cr layout)
             (cairo:restore cr)))))

;;; 2024-3-3
