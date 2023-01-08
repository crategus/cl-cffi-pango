(defpackage :pango-test
  (:use :fiveam :common-lisp)
  (:export #:run!)
  (:import-from :pango   #:+pango-scale+
                         #:+pango-scale-xx-small+
                         #:+pango-scale-x-small+
                         #:+pango-scale-small+
                         #:+pango-scale-medium+
                         #:+pango-scale-large+
                         #:+pango-scale-x-large+
                         #:+pango-scale-xx-large+)
  (:import-from :gobject #:+g-type-boxed+
                         #:define-g-interface
                         #:define-g-enum
                         #:define-g-object-class))

(in-package :pango-test)

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(defun approx-equal (x y &optional (eps 1.0d-1))
  (or (< (abs (- x y)) eps)
      (< (abs (- x y)) (* eps (max (abs x) (abs y))))))

(def-suite pango-suite)
(in-suite pango-suite)

(test version
  (is (string= "1.50.6" (pango:version-string))))

;;; --- 2023-1-3 ---------------------------------------------------------------
