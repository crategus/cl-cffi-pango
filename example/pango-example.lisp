(defpackage :pango-example
  (:use :common-lisp)
  (:import-from :cairo #:with-cairo-context-for-image-surface)
  (:import-from :pango #:+pango-scale+)
  (:export #:png-image-draw
           #:svg-draw
           #:draw-text-centered
           #:draw-text-metrics
           #:draw-text-soulmate

           #:draw-cairo-rendering
  ))

(in-package :pango-example)

(defun sys-path (filename &optional (system :cl-cffi-pango))
  (asdf:system-relative-pathname system filename))

;;; --- 2023-1-17 --------------------------------------------------------------
