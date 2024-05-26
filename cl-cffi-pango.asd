;;; ----------------------------------------------------------------------------
;;; cl-cffi-pango.asd
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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

(defsystem :cl-cffi-pango
  :name "cl-cffi-gtk-pango"
  :version "0.6.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :components
  ((:module src
    :serial t
    :components
    ((:file "pango.package")
     (:file "pango.init")
     (:file "pango.version")
     (:file "pango.matrix")
     (:file "pango.utils")
     (:file "pango.color")
     (:file "pango.script")
     (:file "pango.glyph")
     (:file "pango.gravity")
     (:file "pango.font-description")
     (:file "pango.font-metrics")
     (:file "pango.font")
     (:file "pango.attributes")
     (:file "pango.tab-array")
     (:file "pango.bidirectional")
     (:file "pango.renderer")
     (:file "pango.item")
     (:file "pango.coverage")
     (:file "pango.context")
     (:file "pango.layout")
     (:file "pango.cairo-rendering")
     (:file "pango.markup")
    )))
  :in-order-to ((asdf:test-op (test-op "cl-cffi-pango/test")))
  :depends-on (:cl-cffi-glib
               :cl-cffi-cairo
               :iterate))

;; Definine a test operation for the library

(defsystem :cl-cffi-pango/test
  :name "cl-cffi-pango/test"
  :version "0.6.0"
  :author "Dieter Kaiser"
  :license "MIT"
  :depends-on (:cl-cffi-pango
               :fiveam
               :babel
               :cl-setlocale)
  :perform (test-op (o c)
               (uiop:symbol-call :fiveam :run!
                                 (uiop:find-symbol* :pango-suite
                                                    :pango-test)))
  :components
  ((:module test
    :serial nil
    :components
    ((:file "rtest-pango")
     (:file "rtest-pango-version")
     (:file "rtest-pango-matrix")
     (:file "rtest-pango-utils")
     (:file "rtest-pango-color")
     (:file "rtest-pango-script")
     (:file "rtest-pango-glyph")
     (:file "rtest-pango-gravity")
     (:file "rtest-pango-font-description")
     (:file "rtest-pango-font-metrics")
     (:file "rtest-pango-font")
     (:file "rtest-pango-attributes")
     (:file "rtest-pango-tab-array")
     (:file "rtest-pango-bidirectional")
     (:file "rtest-pango-renderer")
     (:file "rtest-pango-item")
     (:file "rtest-pango-coverage")
     (:file "rtest-pango-context")
     (:file "rtest-pango-layout")
     (:file "rtest-pango-cairo-rendering")
     (:file "rtest-pango-markup")
     (:file "rtest-pango-finish")
    ))))

;;; --- End of file cl-cffi-pango.asd ------------------------------------------
