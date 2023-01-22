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
  (:import-from :cairo   #:with-cairo-context-for-image-surface)
  (:import-from :gobject #:+g-type-boxed+
                         #:define-g-interface
                         #:define-g-enum
                         #:define-g-object-class))

(in-package :pango-test)

;; See https://www.embeddeduse.com/2019/08/26/qt-compare-two-floats/
(defun approx-equal (x y &optional (eps 1.0d-1))
  (or (< (abs (- x y)) eps)
      (< (abs (- x y)) (* eps (max (abs x) (abs y))))))

(defun list-children (gtype)
  (sort (mapcar #'g:type-name (g:type-children gtype))
        #'string<))

(defun list-interfaces (gtype)
  (mapcar #'g:type-name (g:type-interfaces gtype)))

;; A sorted list of the class property names without inherited properties
(defun list-properties (gtype)
  (sort (set-difference (mapcar #'g:param-spec-name
                                (g:object-class-list-properties gtype))
                        (mapcar #'g:param-spec-name
                                (g:object-class-list-properties
                                  (g:type-parent gtype)))
                        :test #'string=)
        #'string<))

(defun list-interface-properties (gtype)
  (mapcar #'g:param-spec-name
          (g:object-interface-list-properties gtype)))

;; A sorted list of the signal names of a class
(defun list-signals (gtype)
  (sort (mapcar #'g:signal-name
                (g:signal-list-ids gtype)) #'string<))

(defun list-flags-item-name (gtype)
  (mapcar #'gobject:flags-item-name
          (gobject:get-flags-items gtype)))

(defun list-flags-item-nick (gtype)
  (mapcar #'gobject:flags-item-nick
          (gobject:get-flags-items gtype)))

(defun list-flags-item-value (gtype)
  (mapcar #'gobject:flags-item-value
          (gobject:get-flags-items gtype)))

(defun list-enum-item-name (gtype)
  (mapcar #'gobject:enum-item-name
          (gobject:get-enum-items gtype)))

(defun list-enum-item-nick (gtype)
  (mapcar #'gobject:enum-item-nick
          (gobject:get-enum-items gtype)))

(defun list-enum-item-value (gtype)
  (mapcar #'gobject:enum-item-value
          (gobject:get-enum-items gtype)))

(def-suite pango-suite)
(in-suite pango-suite)

#-windows
(test version
  (is (string= "1.50.6" (pango:version-string))))

#+windows
(test version
  (is (string= "1.50.7" (pango:version-string))))

;;; --- 2023-1-3 ---------------------------------------------------------------
