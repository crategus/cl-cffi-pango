(in-package :pango-test)

(def-suite pango-color-suite :in pango-suite)
(in-suite pango-color-suite)

;;;     PangoColor

(test pango-color
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoColor") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoColor")
          (g:gtype (cffi:foreign-funcall "pango_color_get_type" :size)))))

;;;     color-red
;;;     color-green
;;;     color-blue

(test pango-color-properties
  (let ((color (make-instance 'pango:color)))
    (is (= 0 (pango:color-red color)))
    (is (= 0 (pango:color-green color)))
    (is (= 0 (pango:color-blue color)))))

;;;     color-new
;;;     color-copy

(test pango-color-new/copy
  (let ((color (pango:color-new :red 1 :green 2 :blue 3)))
    (is (typep color 'pango:color))
    (is (= 1 (pango:color-red color)))
    (is (= 2 (pango:color-green color)))
    (is (= 3 (pango:color-blue color)))
    (is (typep (pango:color-copy color) 'pango:color))
    (is (= 4 (setf (pango:color-red color) 4)))
    (is (= 4 (pango:color-red (pango:color-copy color))))
    (is (= 5 (setf (pango:color-green color) 5)))
    (is (= 5 (pango:color-green (pango:color-copy color))))
    (is (= 6 (setf (pango:color-blue color) 6)))
    (is (= 6 (pango:color-blue (pango:color-copy color))))))

;;;     pango_color_parse
;;;     pango_color_to_string

(test pango-color-parse/to-string
  (let ((color (pango:color-parse "red")))
    (is (= 65535 (pango:color-red color)))
    (is (= 0 (pango:color-green color)))
    (is (= 0 (pango:color-blue color)))
    (is (string= "#ffff00000000" (pango:color-to-string color)))))

;;;     pango_color_parse_with_alpha

(test pango-color-parse-with-alpha
  (multiple-value-bind (color alpha)
      (pango:color-parse-with-alpha "red")
    (is (string= "#ffff00000000" (pango:color-to-string color)))
    (is (= 65535 alpha))))

;;; 2024-3-1
