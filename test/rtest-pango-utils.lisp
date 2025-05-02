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

;; TODO: Create a better example

(test pango-ascent/descent/bearing
  (pango:with-rectangle (rect 10 20 30 40)
    (is (= -20 (pango:ascent rect)))
    (is (=  60 (pango:descent rect)))
    (is (=  10 (pango:lbearing rect)))
    (is (=  40 (pango:rbearing rect)))))

;;;     pango_extents_to_pixels

;; TODO: Create better examples

(test pango-extents-to-pixels.1
  (pango:with-rectangle (inclusive (* 10 pango:+scale+)
                                   (* 20 pango:+scale+)
                                   (* 30 pango:+scale+)
                                   (* 40 pango:+scale+))
      (is-false (pango:extents-to-pixels inclusive nil))
      (is (= 10 (pango:rectangle-x inclusive)))
      (is (= 20 (pango:rectangle-y inclusive)))
      (is (= 30 (pango:rectangle-width inclusive)))
      (is (= 40 (pango:rectangle-height inclusive)))))

(test pango-extents-to-pixels.2
  (pango:with-rectangle (nearest (* 10 pango:+scale+)
                                   (* 20 pango:+scale+)
                                   (* 30 pango:+scale+)
                                   (* 40 pango:+scale+))
      (is-false (pango:extents-to-pixels nil nearest))
      (is (= 10 (pango:rectangle-x nearest)))
      (is (= 20 (pango:rectangle-y nearest)))
      (is (= 30 (pango:rectangle-width nearest)))
      (is (= 40 (pango:rectangle-height nearest)))))

;;; 2025-4-14
