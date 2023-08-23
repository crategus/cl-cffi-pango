(defsystem :pango-example
  :name "pango-example"
  :author "Dieter Kaiser"
  :license "MIT"
  :depends-on (:cl-cffi-pango :cl-cffi-cairo)
  :serial t
  :components
  ((:file "pango-example")
   (:file "png-image-draw")
   (:file "svg-draw")
   (:file "text-centered")
   (:file "text-soulmate")
   (:file "text-metrics")
   (:file "cairo-rendering")
   ))

;;; --- 2023-8-23 --------------------------------------------------------------
