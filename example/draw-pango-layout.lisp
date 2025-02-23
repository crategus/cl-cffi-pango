;;;; Draw Pango Layout
;;;;
;;;; 2025-2-15

(in-package :pango-example)

(defparameter *example-text*
"Zwölf Ägypter
auf der Straße")

(defun draw-pango-layout (cr width height)
  (let* ((layout (pango:cairo-create-layout cr))
         (font nil)
         (font-size (truncate (min (/ height 6) (/ width 18))))
         (factor 1.5)
         (line-spacing 0)
         (indent (truncate (/ font-size 3)))
         (indent2 (truncate (/ indent 3)))
         metrics ascent descent xtext ytext baseline height1)
    (cairo:save cr)
    ;; Paint background
    (cairo:set-source-rgb cr 1.0 1.0 1.0)
    (cairo:paint cr)
    ;; Set font
    (setf (pango:layout-font-description layout)
          (pango:font-description-from-string "Cantarell Bold"))
    ;; Set the font size in Pango units
    (setf (pango:font-description-size (pango:layout-font-description layout))
          (* font-size pango:+scale+))
    ;; Set line spacing
    (setf (pango:layout-line-spacing layout) factor)
    ;; Set text
    (setf (pango:layout-text layout) *example-text*)
    ;; Get font
    (setf font
          (pango:font-map-load-font (pango:cairo-font-map-default)
                                    (pango:layout-context layout)
                                    (pango:layout-font-description layout)))
    ;; Get font metrics
    (setf metrics (pango:font-metrics font (pango:language-default)))
    (setf ascent (pango:pixels (pango:font-metrics-ascent metrics)))
    (setf descent (pango:pixels (pango:font-metrics-descent metrics)))
    (setf height1 (pango:pixels (pango:font-metrics-height metrics)))

    ;; Print information about the font metrics
    (format t "~%~%")
    (format t "        type : ~a~%"
              (pango:cairo-font-map-font-type (pango:cairo-font-map-default)))
    (format t "  resolution : ~a~%"
              (pango:cairo-font-map-resolution (pango:cairo-font-map-default)))
    (format t "        size : ~a~%" font-size)
    (format t "      ascent : ~a~%"
              (pango:pixels (pango:font-metrics-ascent metrics)))
    (format t "     descent : ~a~%"
              (pango:pixels (pango:font-metrics-descent metrics)))
    (format t "      height : ~a~%"
              (pango:pixels (pango:font-metrics-height metrics)))

    ;; Set baseline and line spacing
    (setf baseline (pango:pixels (pango:layout-baseline layout)))
    (setf line-spacing (* factor height1))

    ;; Print information about the layout
    (format t "    baseline : ~a~%"
              (pango:pixels (pango:layout-baseline layout)))
    (format t "    spacing  : ~a~%"
              (pango:pixels (pango:layout-spacing layout)))
    (format t "line-spacing : ~a~%" (pango:layout-line-spacing layout))

    ;; Extents of the Pango layout in pixel units
    (multiple-value-bind (wtext htext) (pango:layout-pixel-size layout)
      ;; Start point for the text
      (setf xtext (- (/ width 2.0)
                     (/ wtext 2.0)))
      (setf ytext (- (/ height 2.0)
                     (/ htext 2.0)))

      (format t "        width : ~a~%" width)
      (format t "       height : ~a~%" height)
      (format t "        xtext : ~a~%" xtext)
      (format t "        ytext : ~a~%" ytext)
      (format t "        wtext : ~a~%" wtext)
      (format t "        htext : ~a~%" htext)

      ;; Set color
      (cairo:set-source-rgb cr 0.5 0.5 0.5)
      ;; Move to the start position of the text
      (cairo:move-to cr xtext ytext)
      ;; Print the example text on the Cario context
      (pango:cairo-show-layout cr layout)

      ;; Draw helper lines with marker and labels

      ;; Set source and line-width
      (cairo:set-source-rgb cr 1.0 0.0 0.0)
      (setf (cairo:hairline cr) t)

      ;; Draw first baseline
      (cairo:move-to cr (- xtext (* 2 indent))
                        (+ ytext baseline))
      (cairo:line-to cr (+ xtext wtext indent)
                        (+ ytext baseline))
      (cairo:stroke cr)

      ;; Draw first ascent line
      (cairo:move-to cr (- xtext (* 2 indent))
                        (+ ytext baseline (- ascent)))
      (cairo:line-to cr (+ xtext wtext indent)
                        (+ ytext baseline (- ascent)))
      (cairo:stroke cr)

      ;; Draw first descent line
      (cairo:move-to cr (- xtext (* 2 indent))
                        (+ ytext baseline descent))
      (cairo:line-to cr (+ xtext wtext indent)
                        (+ ytext baseline descent))
      (cairo:stroke cr)

      ;; Draw second baseline
      (cairo:move-to cr (- xtext (* 2 indent))
                        (+ ytext baseline line-spacing))
      (cairo:line-to cr (+ xtext wtext indent)
                        (+ ytext baseline line-spacing))
      (cairo:stroke cr)

      ;; Draw second ascent line
      (cairo:move-to cr (- xtext (* 2 indent))
                        (+ ytext baseline line-spacing (- ascent)))
      (cairo:line-to cr (+ xtext wtext indent)
                        (+ ytext baseline line-spacing (- ascent)))
      (cairo:stroke cr)

      ;; Second descent line
      (cairo:move-to cr (- xtext (* 2 indent))
                        (+ ytext baseline line-spacing descent))
      (cairo:line-to cr (+ xtext wtext indent)
                        (+ ytext baseline line-spacing descent))
      (cairo:stroke cr)

      ;; Vertical left line for line spacing
      (cairo:move-to cr (+ xtext (- indent) indent2)
                        (+ ytext baseline (- indent2)))
      (cairo:line-to cr (+ xtext (- indent) indent2)
                        (+ ytext baseline line-spacing indent2))
      (cairo:stroke cr)

      ;; Circles at crossings on the left line for line spacing
      (cairo:arc cr
                 (+ xtext (- (* 1 indent)) indent2)
                 (+ ytext baseline)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo:fill cr)
      (cairo:arc cr
                 (+ xtext (- (* 1 indent)) indent2)
                 (+ ytext baseline line-spacing)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo:fill cr)

      ;; Configure font and size for labels
      (setf (pango:layout-font-description layout)
            (pango:font-description-from-string "Courier Normal"))
      ;; Set font size in Pango units
      (setf (pango:font-description-size (pango:layout-font-description layout))
            (max (* 8 pango:+scale+)
                 (* (truncate (/ font-size 10)) pango:+scale+)))

      ;; Label for line spacing (left side)
      (cairo:set-source-rgb cr 0.0 0.0 0.0)
      (setf (pango:layout-text layout) "Line Spacing")
      (pango:cairo-update-layout cr layout)
      (multiple-value-bind (w h) (pango:layout-pixel-size layout)
        (cairo:move-to cr (- xtext (* 2 indent) indent2 w)
                          (+ ytext baseline (/ line-spacing 2) h))
        (pango:cairo-show-layout cr layout))

      ;; Vertical left line for spacing
      (cairo:set-source-rgb cr 1.0 0.0 0.0)
      (cairo:move-to cr (+ xtext (- (* 2 indent)) indent2)
                        (+ ytext baseline descent (- indent2)))
      (cairo:line-to cr (+ xtext (- (* 2 indent)) indent2)
                        (+ ytext baseline line-spacing (- ascent) indent2))
      (cairo:stroke cr)

      ;; Circles at crossings on the left line for spacing
      (cairo:arc cr
                 (+ xtext (- (* 2 indent)) indent2)
                 (+ ytext baseline descent)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo:fill cr)
      (cairo:arc cr
                 (+ xtext (- (* 2 indent)) indent2)
                 (+ ytext baseline (- line-spacing ascent))
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo:fill cr)

      ;; Label spacing (left side)
      (cairo:set-source-rgb cr 0.0 0.0 0.0)
      (setf (pango:layout-text layout) "Spacing")
      (pango:cairo-update-layout cr layout)
      (multiple-value-bind (w h) (pango:layout-pixel-size layout)
      (cairo:move-to cr (- xtext (* 2 indent) indent2 w)
                        (+ ytext baseline descent
                                 (/ (- line-spacing ascent descent h) 2 )))
      (pango:cairo-show-layout cr layout))

      ;; Vertical right line
      (cairo:set-source-rgb cr 1.0 0.0 0.0)
      (cairo:move-to cr (+ xtext wtext indent (- indent2))
                        (- (+ ytext baseline) ascent indent2))
      (cairo:line-to cr (+ xtext wtext indent (- indent2))
                        (+ ytext baseline line-spacing descent indent2))
      (cairo:stroke cr)

      ;; Circles at crosssings on the right line
      (cairo:arc cr
                 (+ xtext wtext indent (- indent2))
                 (+ ytext baseline (- ascent))
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo:fill cr)
      (cairo:arc cr
                 (+ xtext wtext indent (- indent2))
                 (+ baseline ytext)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo:fill cr)
      (cairo:arc cr
                 (+ xtext wtext indent (- indent2))
                 (+ baseline ytext descent)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo:fill cr)
      (cairo:arc cr
                 (+ xtext wtext indent (- indent2))
                 (+ ytext baseline line-spacing descent)
                 (min 5 (truncate (/ indent2 2)))
                 0
                 (* 2 pi))
      (cairo:fill cr)

      ;; Label Ascent
      (cairo:set-source-rgb cr 0.0 0.0 0.0)
      (setf (pango:layout-text layout) "Ascent")
      (pango:cairo-update-layout cr layout)
      (multiple-value-bind (w h) (pango:layout-pixel-size layout)
        (declare (ignore w))
        (cairo:move-to cr (+ xtext wtext (* 1.5 indent))
                          (+ ytext baseline (- (/ ascent 2)) (- (/ h 2))))
        (pango:cairo-show-layout cr layout))

      ;; Label Descent
      (cairo:set-source-rgb cr 0.0 0.0 0.0)
      (setf (pango:layout-text layout) "Descent")
      (pango:cairo-update-layout cr layout)
      (multiple-value-bind (w h) (pango:layout-pixel-size layout)
        (declare (ignore w))
        (cairo:move-to cr (+ xtext wtext (* 1.5 indent))
                          (+ ytext baseline (/ descent 2) (- (/ h 2))))
        (pango:cairo-show-layout cr layout))

      ;; Label Heigth (right)
      (cairo:set-source-rgb cr 0.0 0.0 0.0)
      (setf (pango:layout-text layout) "Height")
      (pango:cairo-update-layout cr layout)
      (multiple-value-bind (w h) (pango:layout-pixel-size layout)
        (declare (ignore w))
        (cairo:move-to cr (+ xtext wtext (* 1.5 indent))
                          (+ ytext (+ baseline line-spacing)
                                   (- (/ height1 2)) (- (/ h 2))))
        (pango:cairo-show-layout cr layout))

      ;; Restore the Cairo context
      (cairo:restore cr))))
