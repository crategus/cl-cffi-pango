(in-package :pango-test)

(def-suite pango-utils-suite :in pango-suite)
(in-suite pango-utils-suite)

;;;     PANGO_SCALE

(test pango-scale
  (is (= 1024 pango:+scale+)))

;;;     PANGO_PIXELS

(test pango-pixels
  (is (= 1 (pango:pixels pango:+scale+))))

;;;     PANGO_PIXELS_FLOOR
;;;     PANGO_PIXELS_CEIL
;;;     PANGO_UNITS_ROUND

;;;     pango_units_to_double
;;;     pango_units_from_double

;;;     PANGO_ASCENT
;;;     PANGO_DESCENT
;;;     PANGO_LBEARING
;;;     PANGO_RBEARING

;;;     pango_extents_to_pixels

(test pango-extents-to-pixels.1
  (cffi:with-foreign-object (inclusive '(:struct pango:rectangle))
    (cffi:with-foreign-slots ((pango::x pango::y pango::width pango::height)
                              inclusive (:struct pango:rectangle))
      (setf pango::x (* 10 pango:+scale+))
      (setf pango::y (* 20 pango:+scale+))
      (setf pango::width (* 30 pango:+scale+))
      (setf pango::height (* 40 pango:+scale+))
      (is-false (pango:extents-to-pixels inclusive (cffi:null-pointer)))
      (is (= 10 pango::x))
      (is (= 20 pango::y))
      (is (= 30 pango::width))
      (is (= 40 pango::height)))))

(test pango-extents-to-pixels.2
  (cffi:with-foreign-object (nearest '(:struct pango:rectangle))
    (cffi:with-foreign-slots ((pango::x pango::y pango::width pango::height)
                              nearest (:struct pango:rectangle))
      (setf pango::x (* 10 pango:+scale+))
      (setf pango::y (* 20 pango:+scale+))
      (setf pango::width (* 30 pango:+scale+))
      (setf pango::height (* 40 pango:+scale+))
      (is-false (pango:extents-to-pixels (cffi:null-pointer) nearest))
      (is (= 10 pango::x))
      (is (= 20 pango::y))
      (is (= 30 pango::width))
      (is (= 40 pango::height)))))

;;; 2024-3-3
