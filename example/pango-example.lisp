(defpackage :pango-example
  (:use :common-lisp)
  (:import-from :cairo)
  (:import-from :pango #:+pango-scale+)
  (:export #:pango-png-image-draw
           #:pango-svg-draw
           #:pango-draw-text-centered
           #:pango-draw-text-metrics
           #:pango-draw-text-soulmate
           #:pango-draw-cairo-rendering
  ))

(in-package :pango-example)

;; Ensure directory for the output of test results
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ensure-directories-exist
      (asdf:system-relative-pathname :pango-example "out/")))

(defun sys-path (path &optional (system :pango-example))
  (asdf:system-relative-pathname system path))

;;; --- 2023-2-12 --------------------------------------------------------------
