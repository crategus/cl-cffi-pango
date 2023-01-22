;;; ----------------------------------------------------------------------------
;;; cl-cffi-pango.asd
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(defsystem :cl-cffi-pango
  :name "cl-cffi-gtk-pango"
  :version "0.1.0"                              ; Version of Pango Library
  :author "Dieter Kaiser"
  :license "LLGPL"
  :components
  ((:module src
    :serial t
    :components
    ((:file "pango.package")
     (:file "pango.init")
     (:file "pango.version")
     (:file "pango.script")
     (:file "pango.glyph")
     (:file "pango.vertical-text")
     (:file "pango.font")
     (:file "pango.attributes")
     (:file "pango.tab-array")
     (:file "pango.bidirectional")
     (:file "pango.renderer")
     (:file "pango.rendering")
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
  :depends-on (:cl-cffi-pango
               :fiveam)
  :perform (test-op (o c)
               (uiop:symbol-call :fiveam :run!
                                 (uiop:find-symbol* :pango-suite
                                                    :pango-test)))
  :components
  ((:module test
    :serial nil
    :components
    ((:file "rtest-pango")
     (:file "rtest-pango-attributes")
;     (:file "pango.tab-array")
;     (:file "pango.script")
;     (:file "pango.bidirectional")
;     (:file "pango.renderer")
;     (:file "pango.glyph")
     (:file "rtest-pango-rendering")
;     (:file "pango.coverage")
     (:file "rtest-pango-vertical-text")
     (:file "rtest-pango-font")
     (:file "rtest-pango-context")
     (:file "rtest-pango-layout")
     (:file "rtest-pango-cairo-rendering")
;     (:file "pango.markup")
    ))))

;; Examples for the Pango library

(asdf:defsystem :cl-cffi-pango/example
  :name "cl-cffi-pango/example"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :pathname "example/"
  :serial t
  :depends-on (:cl-cffi-pango :cl-cffi-cairo)
  :components ((:file "pango-example")

               (:file "png-image-draw")
               (:file "svg-draw")

               (:file "text-centered")
               (:file "text-soulmate")
               (:file "text-metrics")

               (:file "cairo-rendering")

               ))

;;; --- End of file cl-cffi-pango.asd ------------------------------------------
