;;; ----------------------------------------------------------------------------
;;; pango.init.lisp
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(in-package :pango)

(glib-init:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library pango
      ((:and :unix (:not :darwin)) "libpango-1.0.so.0")
      (:darwin (:or "libpango-1.0.0.dylib" "libpango-1.0.dylib"))
      (:windows "libpango-1.0-0.dll")
      (t (:default "libgpango-1.0"))))

  (cffi:use-foreign-library pango))

(glib-init:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library pangocairo
      ((:and :unix (:not :darwin)) "libpangocairo-1.0.so.0")
      (:darwin (:or "libpangocairo-1.0.0.dylib" "libpangocairo-1.0.dylib"))
      (:windows "libpangocairo-1.0-0.dll")
      (t (:default "libgpangocairo-1.0"))))

  (cffi:use-foreign-library pangocairo))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :pango *features*))

(defparameter +pango-version+ (cffi:foreign-funcall "pango_version" :int))
(defparameter +pango-major-version+
              (truncate (/ (cffi:foreign-funcall "pango_version" :int) 10000)))
(defparameter +pango-minor-version+
              (- (truncate (/ (cffi:foreign-funcall "pango_version" :int) 100))
                 (* (truncate (/ (cffi:foreign-funcall "pango_version" :int)
                                 10000))
                    100)))

(glib-init:push-library-version-features
  pango
  +pango-major-version+ +pango-minor-version+
  1 42  ;       12.03.2018
  1 44  ;       27.07.2019
  1 46  ;       10.08.2020
  1 48  ;       08.11.2020
  1 50  ;       03.12.2021
)

(glib-init:require-library-version
  "Pango"
  1 42  ; Since 12.03.2018
  +pango-major-version+
  +pango-minor-version+)

;;; --- End of file pango.init.lisp --------------------------------------------
