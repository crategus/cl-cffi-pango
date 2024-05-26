(in-package :pango-test)

(def-suite pango-matrix-suite :in pango-suite)
(in-suite pango-matrix-suite)

;;;     PangoRectangle

(test pango-with-rectangle.1
  (pango:with-rectangle (rect)
    (is (= 0 (pango:rectangle-x rect)))
    (is (= 0 (pango:rectangle-y rect)))
    (is (= 0 (pango:rectangle-width rect)))
    (is (= 0 (pango:rectangle-height rect)))))

(test pango-with-rectangle.1
  (pango:with-rectangle (rect 1 2 3 4)
    (is (= 1 (pango:rectangle-x rect)))
    (is (= 2 (pango:rectangle-y rect)))
    (is (= 3 (pango:rectangle-width rect)))
    (is (= 4 (pango:rectangle-height rect)))))

(test pango-with-rectangles
  (pango:with-rectangles (rect1 (rect2 1 2 3 4))

    (is (= 0 (pango:rectangle-x rect1)))
    (is (= 0 (pango:rectangle-y rect1)))
    (is (= 0 (pango:rectangle-width rect1)))
    (is (= 0 (pango:rectangle-height rect1)))

    (is (= 1 (pango:rectangle-x rect2)))
    (is (= 2 (pango:rectangle-y rect2)))
    (is (= 3 (pango:rectangle-width rect2)))
    (is (= 4 (pango:rectangle-height rect2)))))

;;;     PangoMatrix

(test pango-matrix-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoMatrix"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoMatrix")
          (g:gtype (cffi:foreign-funcall "pango_matrix_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:matrix
          (glib:symbol-for-gtype "PangoMatrix"))))

;;;     PANGO_MATRIX_INIT
;;;     pango:matrix-new
;;;     pango_matrix_copy
;;;     pango:matrix-to-float

(test pango-matrix-new/copy/init/to-float
  (let* ((matrix1 (pango:matrix-init))
         (matrix2 (pango:matrix-new :xx 1 :yy 2))
         (matrix3 (pango:matrix-copy matrix2)))
    (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 0.0d0 0.0d0)
               (pango:matrix-to-float matrix1)))
    (is (equal '(1.0d0 0.0d0 0.0d0 2.0d0 0.0d0 0.0d0)
               (pango:matrix-to-float matrix2)))
    (is (equal '(1.0d0 0.0d0 0.0d0 2.0d0 0.0d0 0.0d0)
               (pango:matrix-to-float matrix3)))))

;;;     pango_matrix_translate

(test pango-matrix-translate
  (let ((matrix (pango:matrix-init)))
    (is (equal '(1.0d0 0.0d0 0.0d0 1.0d0 2.0d0 0.5d0)
               (pango:matrix-to-float (pango:matrix-translate matrix 2 1/2))))))

;;;     pango_matrix_scale

(test pango-matrix-scale
  (let ((matrix (pango:matrix-init)))
    (is (equal '(2.0d0 0.0d0 0.0d0 0.5d0 0.0d0 0.0d0)
               (pango:matrix-to-float (pango:matrix-scale matrix 2 1/2))))))

;;;     pango_matrix_rotate

(test pango-matrix-rotate
  (let ((matrix (pango:matrix-init)))
    (is (every #'approx-equal
               '(0 1 -1 0 0 0)
               (pango:matrix-to-float (pango:matrix-rotate matrix 90))))))

;;;     pango_matrix_concat

(test pango-matrix-concat
  (let ((matrix1 (pango:matrix-scale (pango:matrix-init) 2 3))
        (matrix2 (pango:matrix-translate (pango:matrix-init) 4 5)))
    (is (equal '(2.0d0 0.0d0 0.0d0 3.0d0 4.0d0 5.0d0)
               (pango:matrix-to-float (pango:matrix-concat matrix2 matrix1))))
    (is (equal '(2.0d0 0.0d0 0.0d0 3.0d0 0.0d0 0.0d0)
               (pango:matrix-to-float matrix1)))
    (is (equal '(2.0d0 0.0d0 0.0d0 3.0d0 4.0d0 5.0d0)
               (pango:matrix-to-float matrix2)))))

;;;     pango_matrix_transform_point

(test pango-matrix-transform-point.1
  (let ((matrix (pango:matrix-scale (pango:matrix-init) 2 3)))
    (is (equal '(6.0d0 12.0d0)
               (multiple-value-list
                   (pango:matrix-transform-point matrix 3 4))))))

(test pango-matrix-transform-point.2
  (let ((matrix (pango:matrix-translate (pango:matrix-init) 2 3)))
    (is (equal '(5.0d0 7.0d0)
               (multiple-value-list
                   (pango:matrix-transform-point matrix 3 4))))))

;;;     pango_matrix_transform_distance

(test pango-matrix-transform-distance.1
  (let ((matrix (pango:matrix-scale (pango:matrix-init) 2 3)))
    (is (equal '(6.0d0 12.0d0)
               (multiple-value-list
                   (pango:matrix-transform-distance matrix 3 4))))))

(test pango-matrix-transform-distance.2
  (let ((matrix (pango:matrix-translate (pango:matrix-init) 2 3)))
    (is (equal '(3.0d0 4.0d0)
               (multiple-value-list
                   (pango:matrix-transform-distance matrix 3 4))))))

;;;     pango_matrix_transform_rectangle

(test pango-matrix-transform-rectangle
  (pango:with-rectangle (rect 0 0 2 3)
    (let ((matrix (pango:matrix-scale (pango:matrix-init) 2 3))
          rect1)

      (is (cffi:pointer-eq (setf rect1
                                 (pango:matrix-transform-rectangle matrix rect))
                           rect))

      (is (= 0 (pango:rectangle-x rect)))
      (is (= 0 (pango:rectangle-y rect)))
      (is (= 4 (pango:rectangle-width rect)))
      (is (= 9 (pango:rectangle-height rect)))

      (is (= 0 (pango:rectangle-x rect1)))
      (is (= 0 (pango:rectangle-y rect1)))
      (is (= 4 (pango:rectangle-width rect1)))
      (is (= 9 (pango:rectangle-height rect1))))))

;;;     pango_matrix_transform_pixel_rectangle

(test pango-matrix-transform-pixel-rectangle
  (pango:with-rectangle (rect 0 0 2 3)
    (let ((matrix (pango:matrix-scale (pango:matrix-init) 2 3))
          rect1)

      (is (cffi:pointer-eq (setf rect1
                                 (pango:matrix-transform-pixel-rectangle matrix
                                                                         rect))
                           rect))

      (is (= 0 (pango:rectangle-x rect)))
      (is (= 0 (pango:rectangle-y rect)))
      (is (= 4 (pango:rectangle-width rect)))
      (is (= 9 (pango:rectangle-height rect)))

      (is (= 0 (pango:rectangle-x rect1)))
      (is (= 0 (pango:rectangle-y rect1)))
      (is (= 4 (pango:rectangle-width rect1)))
      (is (= 9 (pango:rectangle-height rect1))))))

;;;     pango_matrix_get_font_scale_factor

(test pango-matrix-font-scale-factor
  (let ((matrix (pango:matrix-scale (pango:matrix-init) 2 3)))
    (is (= 3.0d0 (pango:matrix-font-scale-factor matrix)))))

;;;     pango_matrix_get_font_scale_factors

(test pango-matrix-font-scale-factors
  (let ((matrix (pango:matrix-scale (pango:matrix-init) 2 3)))
    (is (equal '(2.0d0 3.0d0)
               (multiple-value-list (pango:matrix-font-scale-factors matrix))))))

;;;     pango_matrix_get_slant_ratio

(test pango-matrix-slant-ratio
  (let ((matrix (pango:matrix-new :xx 1 :xy 0 :yx 2 :yy 1 :x0 0 :y0 0)))
    (is (= 2.0d0 (pango:matrix-slant-ratio matrix)))))

;;; 2024-5-25
