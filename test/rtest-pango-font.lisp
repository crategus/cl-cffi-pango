(in-package :pango-test)

(def-suite pango-font :in pango-suite)
(in-suite pango-font)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoFont

(test pango-font-class
  ;; Type check
  (is (g:type-is-object "PangoFont"))
  ;; Check the registered name
  (is (eq 'pango:font
          (glib:symbol-for-gtype "PangoFont")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoFont")
          (g:gtype (cffi:foreign-funcall "pango_font_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoFont")))
  ;; Check the children
  #-windows
  (is (equal '("PangoFcFont")
             (list-children "PangoFont")))
  #+windows
  (is (equal '("PangoWin32Font")
             (list-children "PangoFont")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "PangoFont")))
  ;; Check the properties
  (is (equal '()
             (list-properties "PangoFont")))
  ;; Check the signals
  (is (equal '()
             (list-signals "PangoFont")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "PangoFont" PANGO-FONT
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "pango_font_get_type")
                               NIL)
             (gobject:get-g-type-definition "PangoFont"))))

;;;     PangoFontFamily

(test pango-font-family-class
  ;; Type check
  (is (g:type-is-object "PangoFontFamily"))
  ;; Check the registered name
  (is (eq 'pango:font-family
          (glib:symbol-for-gtype "PangoFontFamily")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoFontFamily")
          (g:gtype (cffi:foreign-funcall "pango_font_family_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoFontFamily")))
  ;; Check the children
  #-windows
  (if *first-run-pango-test*
      (is (equal '()
                 (list-children "PangoFontFamily")))
      (is (equal '("PangoFcFamily")
                 (list-children "PangoFontFamily"))))
  #+windows
  (is (equal '("PangoWin32Family")
             (list-children "PangoFontFamily")))
  ;; Check the interfaces
  (is (equal '("GListModel")
             (list-interfaces "PangoFontFamily")))
  ;; Check the properties
  (is (equal '("item-type" "n-items")
             (list-properties "PangoFontFamily")))
  ;; Check the signals
  (is (equal '()
             (list-signals "PangoFontFamily")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "PangoFontFamily" PANGO-FONT-FAMILY
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel") :TYPE-INITIALIZER
                                "pango_font_family_get_type")
                               ((ITEM-TYPE PANGO-FONT-FAMILY-ITEM-TYPE
                                 "item-type" "GType" T NIL)
                                (N-ITEMS PANGO-FONT-FAMILY-N-ITEMS "n-items"
                                 "guint" T NIL)))
             (gobject:get-g-type-definition "PangoFontFamily"))))

;;;     PangoFontFace

(test pango-font-face-class
  ;; Type check
  (is (g:type-is-object "PangoFontFace"))
  ;; Check the registered name
  (is (eq 'pango:font-face
          (glib:symbol-for-gtype "PangoFontFace")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoFontFace")
          (g:gtype (cffi:foreign-funcall "pango_font_face_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoFontFace")))
  ;; Check the children
  #-windows
  (if *first-run-pango-test*
      (is (equal '()
                 (list-children "PangoFontFace")))
      (is (equal '("PangoFcFace")
                 (list-children "PangoFontFace"))))
  #+windows
  (is (equal '("PangoWin32Face")
             (list-children "PangoFontFace")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "PangoFontFace")))
  ;; Check the properties
  (is (equal '()
             (list-properties "PangoFontFace")))
  ;; Check the signals
  (is (equal '()
             (list-signals "PangoFontFace")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "PangoFontFace" PANGO-FONT-FACE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "pango_font_face_get_type")
                               NIL)
             (gobject:get-g-type-definition "PangoFontFace"))))

;;;     PangoFontMap

(test pango-font-map-class
  ;; Type check
  (is (g:type-is-object "PangoFontMap"))
  ;; Check the registered name
  (is (eq 'pango:font-map
          (glib:symbol-for-gtype "PangoFontMap")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoFontMap")
          (g:gtype (cffi:foreign-funcall "pango_font_map_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoFontMap")))
  ;; Check the children
  #-windows
  (is (equal '("PangoFcFontMap")
             (list-children "PangoFontMap")))
  #+windows
  (if *first-run-pango-test*
      (is (equal '("PangoWin32FontMap")
                 (list-children "PangoFontMap"))))
  ;; Check the interfaces
  (is (equal '("GListModel")
             (list-interfaces "PangoFontMap")))
  ;; Check the properties
  (is (equal '("item-type" "n-items")
             (list-properties "PangoFontMap")))
  ;; Check the signals
  (is (equal '()
             (list-signals "PangoFontMap")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "PangoFontMap" PANGO-FONT-MAP
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES
                                ("GListModel") :TYPE-INITIALIZER
                                "pango_font_map_get_type")
                               ((ITEM-TYPE PANGO-FONT-MAP-ITEM-TYPE "item-type"
                                 "GType" T NIL)
                                (N-ITEMS PANGO-FONT-MAP-N-ITEMS "n-items"
                                 "guint" T NIL)))
             (gobject:get-g-type-definition "PangoFontMap"))))

;;;     PangoFontset

(test pango-fontset-class
  ;; Type check
  (is (g:type-is-object "PangoFontset"))
  ;; Check the registered name
  (is (eq 'pango:fontset
          (glib:symbol-for-gtype "PangoFontset")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoFontset")
          (g:gtype (cffi:foreign-funcall "pango_fontset_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoFontset")))
  ;; Check the children
  #-windows
  (is (equal '("PangoFcFontset")
             (list-children "PangoFontset")))
  #+windows
  (is (equal '("PangoFontsetSimple")
             (list-children "PangoFontset")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "PangoFontset")))
  ;; Check the properties
  (is (equal '()
             (list-properties "PangoFontset")))
  ;; Check the signals
  (is (equal '()
             (list-signals "PangoFontset")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "PangoFontset" PANGO-FONTSET
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "pango_fontset_get_type")
                               NIL)
             (gobject:get-g-type-definition "PangoFontset"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_font_describe

(test pango-font-describe
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         desc)
    (is (typep (setf desc
                     (pango:font-describe font))
               'pango:font-description))
    (is (equal '(:FAMILY :STYLE :VARIANT :WEIGHT :STRETCH :SIZE)
               (pango:font-description-set-fields desc)))
    (is-false (pango:font-description-size-is-absolute desc))))

;;;     pango_font_describe_with_absolute_size

(test pango-font-describe-with-absolute-size
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         desc)
    (is (typep (setf desc
                     (pango:font-describe-with-absolute-size font))
               'pango:font-description))
    (is (equal '(:FAMILY :STYLE :VARIANT :WEIGHT :STRETCH :SIZE)
               (pango:font-description-set-fields desc)))
    (is-true (pango:font-description-size-is-absolute desc))))

;;;     pango_font_get_face

(test pango-font-face
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         face)
    (is (typep (setf face (pango:font-face font)) 'pango:font-face))
    #-windows
    (is (string= "Regular" (pango:font-face-face-name face)))
    #+windows
    (is (string= "Normal" (pango:font-face-face-name face)))))

;;;     pango_font_get_coverage

(test pango-font-coverage
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         coverage)
    (is (typep (setf coverage
                     (pango:font-coverage font
                                          (pango:language-default)))
               'pango:coverage))))

;;;     pango_font_has_char

(test pango-font-coverage
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc)))
    (is-true (pango:font-has-char font #\a))
    (is-true (pango:font-has-char font 97))
    (is-true (pango:font-has-char font #\ä))
    (is-true (pango:font-has-char font 228))))

;;;     pango_font_get_glyph_extents

#-windows
(test pango-font-glyph-extents
  (pango:with-rectangles (ink logical)
    (let* ((fontmap (pango:cairo-font-map-default))
           (context (pango:font-map-create-context fontmap))
           (desc (pango:font-description-from-string "Sans 12"))
           (font (pango:font-map-load-font fontmap context desc)))
      (is-false (pango:font-glyph-extents font 20 ink logical))
      ;; ink rectangle
      (is (= 1024 (pango:rectangle-x ink)))
      (is (= -12288 (pango:rectangle-y ink)))
      (is (= 5120 (pango:rectangle-width ink)))
      (is (= 12288 (pango:rectangle-height ink)))
      ;; Get ascent, descent, lbearing, rbearing
      (is (= 12288 (pango:ascent ink)))
      (is (=     0 (pango:descent ink)))
      (is (=  1024 (pango:lbearing ink)))
      (is (=  6144 (pango:rbearing ink)))
      ;; logical rectangle
      (is (= 0 (pango:rectangle-x logical)))
      (is (= -18432 (pango:rectangle-y logical)))
      (is (= 9216 (pango:rectangle-width logical)))
      (is (= 23552 (pango:rectangle-height logical))))))

#+windows
(test pango-font-glyph-extents
  (pango:with-rectangles (ink logical)
    (let* ((fontmap (pango:cairo-font-map-default))
           (context (pango:font-map-create-context fontmap))
           (desc (pango:font-description-from-string "Sans 12"))
           (font (pango:font-map-load-font fontmap context desc)))
      (is-false (pango:font-glyph-extents font 20 ink logical))
      ;; ink rectangle
      (is (= 1024 (pango:rectangle-x ink)))
      (is (= -12288 (pango:rectangle-y ink)))
      (is (= 10240 (pango:rectangle-width ink)))
      (is (= 12288 (pango:rectangle-height ink)))
      ;; Get ascent, descent, lbearing, rbearing
      (is (= 12288 (pango:ascent ink)))
      (is (=     0 (pango:descent ink)))
      (is (=  1024 (pango:lbearing ink)))
      (is (= 11264 (pango:rbearing ink)))
      ;; logical rectangle
      (is (= 0 (pango:rectangle-x logical)))
      (is (= -15360 (pango:rectangle-y logical)))
      (is (= 10240 (pango:rectangle-width logical)))
      (is (= 19456 (pango:rectangle-height logical))))))

#-windows
(test pango-font-glyph-extents.1
  (pango:with-rectangles (ink)
    (let* ((fontmap (pango:cairo-font-map-default))
           (context (pango:font-map-create-context fontmap))
           (desc (pango:font-description-from-string "Sans 12"))
           (font (pango:font-map-load-font fontmap context desc)))
      (is-false (pango:font-glyph-extents font 20 ink nil))
      ;; ink rectangle
      (is (= 1024 (pango:rectangle-x ink)))
      (is (= -12288 (pango:rectangle-y ink)))
      (is (= 5120 (pango:rectangle-width ink)))
      (is (= 12288 (pango:rectangle-height ink))))))

#+windows
(test pango-font-glyph-extents.1
  (pango:with-rectangles (ink)
    (let* ((fontmap (pango:cairo-font-map-default))
           (context (pango:font-map-create-context fontmap))
           (desc (pango:font-description-from-string "Sans 12"))
           (font (pango:font-map-load-font fontmap context desc)))
      (is-false (pango:font-glyph-extents font 20 ink nil))
      ;; ink rectangle
      (is (= 1024 (pango:rectangle-x ink)))
      (is (= -12288 (pango:rectangle-y ink)))
      (is (= 10240 (pango:rectangle-width ink)))
      (is (= 12288 (pango:rectangle-height ink))))))

#-windows
(test pango-font-glyph-extents.2
  (pango:with-rectangles (logical)
    (let* ((fontmap (pango:cairo-font-map-default))
           (context (pango:font-map-create-context fontmap))
           (desc (pango:font-description-from-string "Sans 12"))
           (font (pango:font-map-load-font fontmap context desc)))
      (is-false (pango:font-glyph-extents font 20 nil logical))
      ;; logical rectangle
      (is (= 0 (pango:rectangle-x logical)))
      (is (= -18432 (pango:rectangle-y logical)))
      (is (= 9216 (pango:rectangle-width logical)))
      (is (= 23552 (pango:rectangle-height logical))))))

#+windows
(test pango-font-glyph-extents.2
  (pango:with-rectangles (logical)
    (let* ((fontmap (pango:cairo-font-map-default))
           (context (pango:font-map-create-context fontmap))
           (desc (pango:font-description-from-string "Sans 12"))
           (font (pango:font-map-load-font fontmap context desc)))
      (is-false (pango:font-glyph-extents font 20 nil logical))
      ;; logical rectangle
      (is (= 0 (pango:rectangle-x logical)))
      (is (= -15360 (pango:rectangle-y logical)))
      (is (= 10240 (pango:rectangle-width logical)))
      (is (= 19456 (pango:rectangle-height logical))))))

;;;     pango_font_get_metrics

(test pango-font-metrics
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc)))
    (is (typep (pango:font-metrics font (pango:language-default))
               'pango:font-metrics))))

;;;     pango_font_get_font_map

(test pango-font-font-map
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc)))
    (is (typep (pango:font-font-map font) 'pango:font-map))))

;;;     pango_font_get_features
;;;     pango_font_get_hb_font

;;;     pango_font_get_languages                           Since 1.50

(test pango-font-languages
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc)))
  (is (every (lambda (x) (typep x 'pango:language))
             (pango:font-languages font)))))

;;;     pango_font_serialize                               Since 1.50
;;;     pango_font_deserialize                             Since 1.50

#-windows
(test pango-font-serialize/deserialize
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         bytes)
  (is (typep (setf bytes
                   (pango:font-serialize font)) 'g:bytes))
  (is (string=
"{
  \"description\" : \"Noto Sans 12\",
  \"checksum\" : \"89c3c497f618fdaa0b2d1e98fef93582f28c71debd2c4a8cdf41f190ced2909d\",
  \"matrix\" : [
    1,
    -0,
    -0,
    1,
    0,
    0
  ]
}"
               (cffi:foreign-string-to-lisp (g:bytes-data bytes)
                                            :count (g:bytes-size bytes))))
  (is (typep (pango:font-deserialize context bytes) 'pango:font))
))

#+windows
(test pango-font-serialize/deserialize
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         bytes)
  (is (typep (setf bytes
                   (pango:font-serialize font)) 'g:bytes))
  (is (string=
"{
  \"description\" : \"DejaVu Sans 12\",
  \"checksum\" : \"7da195a74c55bef988d0d48f9508bd5d849425c1770dba5d7bfc6ce9ed848954\"
}"
               (cffi:foreign-string-to-lisp (g:bytes-data bytes)
                                            :count (g:bytes-size bytes))))
  (is (typep (pango:font-deserialize context bytes) 'pango:font))
))

;;;     pango_font_family_get_name

(test pango-font-family-name
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif")))
    ;; Get the properties for the interface
    (is (eq (g:gtype "PangoFontFace") (g:list-model-item-type family)))
    #-windows
    (is (= 4 (g:list-model-n-items family)))
    #+windows
    (is (= 8 (g:list-model-n-items family)))
    ;; Get the properties for PangoFontFamily
    ;; TODO: This seems to be a bug in the C implementation. We do not get
    ;; an integer with the GType, but a pointer with the address of the GType
    #-windows
    (is (eq (g:gtype "PangoFontFace")
            (g:gtype (cffi:pointer-address
                         (pango:font-family-item-type family)))))
    #+windows
    (is (eq (g:gtype "PangoFont")
            (g:gtype (cffi:pointer-address
                         (pango:font-family-item-type family)))))
    #-windows
    (is (= 4 (pango:font-family-n-items family)))
    #+windows
    (is (= 8 (pango:font-family-n-items family)))
    ;; Get the font family name
    (is (string= "Serif" (pango:font-family-name family)))))

;;;     pango_font_family_is_monospace

(test pango-font-family-is-monospace.1
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif")))
    (is-false (pango:font-family-is-monospace family))))

(test pango-font-family-is-monospace.2
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Monospace")))
    (is-true (pango:font-family-is-monospace family))))

;;;     pango_font_family_is_variable

(test pango-font-family-is-variable.1
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif")))
    (is-false (pango:font-family-is-variable family))))

(test pango-font-family-is-variable.2
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Monospace")))
    (is-false (pango:font-family-is-variable family))))

;;;     pango_font_family_list_faces

#-windows
(test pango-font-family-list-faces
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif")))
    (is (= (pango:font-family-n-items family)
           (length (pango:font-family-list-faces family))))
    (is (every (lambda (x) (typep x 'pango:font-face))
               (pango:font-family-list-faces family)))
    (is (equal '("Regular" "Bold" "Italic" "Bold Italic")
               (mapcar #'pango:font-face-face-name
                       (pango:font-family-list-faces family))))))

#+windows
(test pango-font-family-list-faces
  (let* ((fontmap (pango:cairo-font-map-default))
         (family (pango:font-map-family fontmap "Serif")))
    (is (= (pango:font-family-n-items family)
           (length (pango:font-family-list-faces family))))
    (is (every (lambda (x) (typep x 'pango:font-face))
               (pango:font-family-list-faces family)))
    (is (equal '("Normal" "Semi-Condensed" "Italic" "Italic Semi-Condensed"
                 "Bold" "Bold Semi-Condensed" "Bold Italic"
                 "Bold Italic Semi-Condensed")
               (mapcar #'pango:font-face-face-name
                       (pango:font-family-list-faces family))))))

;;;     pango_font_family_get_face

#-windows
(test pango-font-family-face
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif")))
    (is (typep (pango:font-family-face family "Regular") 'pango:font-face))
    (is (typep (pango:font-family-face family nil) 'pango:font-face))))

#+windows
(test pango-font-family-face
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif")))
    (is (typep (pango:font-family-face family "Normal") 'pango:font-face))
    (is (typep (pango:font-family-face family nil) 'pango:font-face))))

;;;     pango_font_face_get_face_name

#-windows
(test pango-font-family-face-name.1
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif"))
         (face (pango:font-family-face family "Regular")))
    (is (typep face 'pango:font-face))
    (is (string= "Regular" (pango:font-face-face-name face)))))

#+windows ; On Windows it is "Normal" and not "Regular"
(test pango-font-family-face-name.1
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif"))
         (face (pango:font-family-face family "Normal")))
    (is (typep face 'pango:font-face))
    (is (string= "Normal" (pango:font-face-face-name face)))))

#-windows
(test pango-font-family-face-name.2
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif"))
         (face (pango:font-family-face family nil)))
    (is (typep face 'pango:font-face))
    (is (string= "Regular" (pango:font-face-face-name face)))))

#+windows ; On Windows it is "Normal" and not "Regular"
(test pango-font-family-face-name.2
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif"))
         (face (pango:font-family-face family nil)))
    (is (typep face 'pango:font-face))
    (is (string= "Normal" (pango:font-face-face-name face)))))

;;;     pango_font_face_list_sizes

;; TODO: Can we create an example with a result different from nil.
;; A result different from nil needs a bitmap font.

#-windows
(test pango-font-face-list-sizes
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif"))
         (face (pango:font-family-face family "Regular")))
    (is (typep face 'pango:font-face))
    (is-false (pango:font-face-list-sizes face))))

#+windows ; On Windows it is not "Regular" but "Normal"
(test pango-font-face-list-sizes
  (let* ((font-map (pango:cairo-font-map-default))
         (family (pango:font-map-family font-map "Serif"))
         (face (pango:font-family-face family "Normal")))
    (is (typep face 'pango:font-face))
    (is-false (pango:font-face-list-sizes face))))

;;;     pango_font_face_describe

;;;     pango_font_face_is_synthesized
;;;     pango_font_face_get_family
;;;
;;;     pango_font_map_create_context

;;;     pango_font_map_load_font

(test pango-font-map-load-font
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         font)
    (is (typep (setf font (pango:font-map-load-font fontmap context desc))
               'pango:font))))

;;;     pango_font_map_load_fontset

(test pango-font-map-load-fontset
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (lang (pango:language-default))
         fontset)
    (is (typep (setf fontset
                     (pango:font-map-load-fontset fontmap context desc lang))
               'pango:fontset))
    (is (typep (pango:fontset-font fontset #\a) 'pango:font))))

;;;     pango_font_map_list_families

(test pango-font-map-list-families
  (let ((fontmap (pango:cairo-font-map-default)))
    (is (every (lambda (x) (typep x 'pango:font-family))
               (pango:font-map-list-families fontmap)))
    (is (every #'stringp
               (mapcar #'pango:font-family-name
                       (pango:font-map-list-families fontmap))))))

;;;     pango_font_map_get_family

#+windows
(test pango-font-map-family
  (let ((fontmap (pango:cairo-font-map-default))
        family)
    (is (typep (setf family
                     (pango:font-map-family fontmap "Serif"))
               'pango:font-family))
    ;; We get a pointer and not a GType?!
    (is (cffi:pointerp (pango:font-family-item-type family)))
    #-windows
    (is (= 4 (pango:font-family-n-items family)))
    #+windows
    (is (= 4 (pango:font-family-n-items family)))))

#+windows
(test pango-font-map-family
  (let ((fontmap (pango:cairo-font-map-default))
        family)
    (is (typep (setf family
                     (pango:font-map-family fontmap "Serif"))
               'pango:font-family))
    ;; We get a pointer and not a GType?!
    (is (cffi:pointerp (pango:font-family-item-type family)))
    (is (= 8 (pango:font-family-n-items family)))))

;;;     pango_font_map_get_serial

(test pango-font-map-serial
  (is (integerp (pango:font-map-serial (pango:cairo-font-map-default)))))

;;;     pango_font_map_changed
;;;     pango_font_map_get_shape_engine_type

;;; ----------------------------------------------------------------------------

;;;     pango_fontset_get_font

#-windows
(test pango-fontset-font
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans Italic 12"))
         (lang (pango:language-default))
         (fontset (pango:font-map-load-fontset fontmap context desc lang))
         font)
    (is (typep (setf font (pango:fontset-font fontset #\a)) 'pango:font))
    (is (string= "Noto Sans Italic 12"
                 (pango:font-description-to-string (pango:font-describe font))))
    (is (typep (setf font (pango:fontset-font fontset #\ä)) 'pango:font))
    (is (string= "Noto Sans Italic 12"
                 (pango:font-description-to-string (pango:font-describe font))))))

#+windows
(test pango-fontset-font
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans Italic 12"))
         (lang (pango:language-default))
         (fontset (pango:font-map-load-fontset fontmap context desc lang))
         font)
    (is (typep (setf font (pango:fontset-font fontset #\a)) 'pango:font))
    (is (string= "DejaVu Sans Oblique 12"
                 (pango:font-description-to-string (pango:font-describe font))))
    (is (typep (setf font (pango:fontset-font fontset #\ä)) 'pango:font))
    (is (string= "DejaVu Sans Oblique 12"
                 (pango:font-description-to-string (pango:font-describe font))))))

;;;     pango_fontset_get_metrics

#-windows
(test pango-fontset-metrics
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans Italic 12"))
         (lang (pango:language-default))
         (fontset (pango:font-map-load-fontset fontmap context desc lang))
         metrics)
    (is (typep (setf metrics
                     (pango:fontset-metrics fontset)) 'pango:font-metrics))
    (is (= 18432 (pango:font-metrics-ascent metrics)))
    (is (=  5120 (pango:font-metrics-descent metrics)))
    (is (= 22528 (pango:font-metrics-height metrics)))
    (is (=  7376 (pango:font-metrics-approximate-char-width metrics)))
    (is (=  9216 (pango:font-metrics-approximate-digit-width metrics)))
    (is (=  1024 (pango:font-metrics-underline-thickness metrics)))
    (is (= -1024 (pango:font-metrics-underline-position metrics)))
    (is (=  1024 (pango:font-metrics-strikethrough-thickness metrics)))
    (is (=  6144 (pango:font-metrics-strikethrough-position metrics)))))

#+windows
(test pango-fontset-metrics
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans Italic 12"))
         (lang (pango:language-default))
         (fontset (pango:font-map-load-fontset fontmap context desc lang))
         metrics)
    (is (typep (setf metrics
                     (pango:fontset-metrics fontset)) 'pango:font-metrics))
    (is (= 15360 (pango:font-metrics-ascent metrics)))
    (is (=  4096 (pango:font-metrics-descent metrics)))
    (is (= 19456 (pango:font-metrics-height metrics)))
    (is (=  8560 (pango:font-metrics-approximate-char-width metrics)))
    (is (= 10240 (pango:font-metrics-approximate-digit-width metrics)))
    (is (=  1024 (pango:font-metrics-underline-thickness metrics)))
    (is (= -2048 (pango:font-metrics-underline-position metrics)))
    (is (=  1024 (pango:font-metrics-strikethrough-thickness metrics)))
    (is (=  5120 (pango:font-metrics-strikethrough-position metrics)))))

;;;     PangoFontsetForeachFunc
;;;     pango_fontset_foreach

(test pango-fontset-foreach
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans Italic 12"))
         (lang (pango:language-default))
         (fontset (pango:font-map-load-fontset fontmap context desc lang)))
    (let ((count 0))
      (pango:fontset-foreach fontset
                             (lambda (fontset font)
                               (is (typep fontset 'pango:fontset))
                               (is (typep font 'pango:font))
                               (incf count)
                               nil))
        #-windows
        (is (= 176 count))
        #+windows
        (is (= 5 count)))))

;;; 2024-3-4
