(in-package :pango-test)

(def-suite pango-cairo-rendering :in pango-suite)
(in-suite pango-cairo-rendering)

(defvar *verbose-pango-cairo-rendering* nil)

;;; -- Types and Values --------------------------------------------------------

;;;     PangoCairoFont

(test pango-cairo-font-interface
  ;; Check type
  (is (g:type-is-interface "PangoCairoFont"))
  ;; Check registered name
  (is (eq 'pango:cairo-font
          (glib:symbol-for-gtype "PangoCairoFont")))
  ;; Check type initializer
  (is (eq (g:gtype "PangoCairoFont")
          (g:gtype (cffi:foreign-funcall "pango_cairo_font_get_type" :size))))
  ;; Check interface prerequisites
  (is (equal '("PangoFont")
             (glib-test:list-interface-prerequisites "PangoCairoFont")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "PangoCairoFont")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "PangoCairoFont")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "PangoCairoFont" PANGO:CAIRO-FONT
                      (:EXPORT T
                       :TYPE-INITIALIZER "pango_cairo_font_get_type"))
             (gobject:get-gtype-definition "PangoCairoFont"))))

;;;     PangoCairoFontMap

(test pango-cairo-font-map-interface
  ;; Check type
  (is (g:type-is-interface "PangoCairoFontMap"))
  ;; Check registered name
  (is (eq 'pango:cairo-font-map
          (glib:symbol-for-gtype "PangoCairoFontMap")))
  ;; Check type initializer
  (is (eq (g:gtype "PangoCairoFontMap")
          (g:gtype (cffi:foreign-funcall "pango_cairo_font_map_get_type"
                                         :size))))
  ;; Check interface prerequisites
  (is (equal '("PangoFontMap")
             (glib-test:list-interface-prerequisites "PangoCairoFontMap")))
  ;; Check interface properties
  (is (equal '()
             (glib-test:list-interface-properties "PangoCairoFontMap")))
  ;; Check interface signals
  (is (equal '()
             (glib-test:list-signals "PangoCairoFontMap")))
  ;; Check interface definition
  (is (equal '(GOBJECT:DEFINE-GINTERFACE "PangoCairoFontMap" PANGO:CAIRO-FONT-MAP
                      (:EXPORT T
                       :TYPE-INITIALIZER "pango_cairo_font_map_get_type"))
             (gobject:get-gtype-definition "PangoCairoFontMap"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_cairo_font_map_get_default
;;;     pango_cairo_font_map_set_default

(test pango-cairo-font-map-default
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (is (typep (setf (pango:cairo-font-map-default)
                       (pango:cairo-font-map-new)) 'pango:font-map))
      (is (typep (pango:cairo-font-map-default) 'pango:font-map))
      (is (typep (pango:cairo-font-map-default) 'pango:font-map))
      (is (typep (pango:cairo-font-map-default) 'pango:font-map)))))

;;;     pango_cairo_font_map_new

(test pango-cairo-font-map-new
  (glib-test:with-check-memory (fontmap)
    (is (typep (setf fontmap (pango:cairo-font-map-new)) 'pango:font-map))))

;;;     pango_cairo_font_map_new_for_font_type

(test pango-cairo-font-map-new-for-font-type
  (glib-test:with-check-memory ()
    (is-false (pango:cairo-font-map-new-for-font-type :toy))
    (is-true  (pango:cairo-font-map-new-for-font-type :ft))
    #-windows
    (is-false (pango:cairo-font-map-new-for-font-type :win32))
    #+windows
    (is-true  (pango:cairo-font-map-new-for-font-type :win32))
    (is-false (pango:cairo-font-map-new-for-font-type :quartz))
    (is-false (pango:cairo-font-map-new-for-font-type :user))))

;;;     pango_cairo_font_map_get_font_type

#-windows
(test pango-cairo-font-map-font-type
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (is (eq :ft (pango:cairo-font-map-font-type (pango:cairo-font-map-default))))
      (is (eq :ft (pango:cairo-font-map-font-type (pango:cairo-font-map-new)))))))

#+windows
(test pango-cairo-font-map-font-type
  (is (eq :win32
          (pango:cairo-font-map-font-type (pango:cairo-font-map-default))))
  (is (eq :win32 (pango:cairo-font-map-font-type (pango:cairo-font-map-new)))))

;;;     pango_cairo_font_map_set_resolution
;;;     pango_cairo_font_map_get_resolution

(test pango-cairo-font-map-resolution
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 1)
      (let ((fontmap (pango:cairo-font-map-default)))
        (is (=  96.0d0 (pango:cairo-font-map-resolution fontmap)))
        (is (= 200.0d0 (setf (pango:cairo-font-map-resolution fontmap) 200)))
        (is (= 200.0d0 (pango:cairo-font-map-resolution fontmap)))
        ;; Restore the default resolution
        (is (=  96.0d0 (setf (pango:cairo-font-map-resolution fontmap) 96.0d0)))))))

;;;     pango_cairo_font_map_create_context                not exported

;;;     pango_cairo_font_get_scaled_font

#+nil
(test pango-cairo-font-scaled-font
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default)))
        (desc (pango:font-description-from-string "Sans"))
        font)
    (is (typep (setf font
                     (pango:context-load-font context desc)) 'pango:font))
    ;; font is of type pango:font but not pango:cairo-font
    (is-false (pango:cairo-font-scaled-font font))
))

;;;     pango_cairo_context_set_resolution
;;;     pango_cairo_context_get_resolution

(test pango-cairo-context-resolution
  (when *first-run-testsuite*
    (glib-test:with-check-memory (context :strong 1)
      (is (typep (setf context
                       (pango:font-map-create-context (pango:cairo-font-map-default)))
                 'pango:context))
      (is (= -1.0d0 (pango:cairo-context-resolution context)))
      (is (= 96.0d0 (setf (pango:cairo-context-resolution context) 96)))
      (is (= 96.0d0 (pango:cairo-context-resolution context))))))

;;;     pango_cairo_context_set_font_options
;;;     pango_cairo_context_get_font_options

(test pango-cairo-context-font-options
  (when *first-run-testsuite*
    (glib-test:with-check-memory (context :strong 1)
      (let ((options (cairo:font-options-create)))
        (is (typep (setf context
                         (pango:font-map-create-context (pango:cairo-font-map-default)))
                   'pango:context))
        (is-false (pango:cairo-context-font-options context))
        ;; Set the font options
        (is (cffi:pointer-eq options
                             (setf (pango:cairo-context-font-options context)
                                   options)))
        (is (cffi:pointerp (pango:cairo-context-font-options context)))
        ;; Unset the font options
        (is-false (setf (pango:cairo-context-font-options context) nil))
        (is-false (pango:cairo-context-font-options context))
        (is-false (cairo:font-options-destroy options))))))

;;;     PangoCairoShapeRendererFunc                        not exported
;;;     pango_cairo_context_set_shape_renderer             not exported
;;;     pango_cairo_context_get_shape_renderer             not exported

;;;     pango_cairo_create_context

(test pango-cairo-create-context
  (glib-test:with-check-memory (context)
    (cairo:with-context-for-image-surface (cr :rgb24 200 400)
      (is (typep (setf context
                       (pango:cairo-create-context cr)) 'pango:context)))))

;;;     pango_cairo_update_context

(test pango-cairo-update-context
  (glib-test:with-check-memory (context)
    (cairo:with-context-for-image-surface (cr :rgb24 200 400)
      (is (typep (setf context (pango:cairo-create-context cr)) 'pango:context))
      (is-false (pango:cairo-update-context cr context)))))

;;;     pango_cairo_create_layout

(test pango-cairo-create-layout
  (glib-test:with-check-memory (layout)
    (cairo:with-context-for-image-surface (cr :rgb24 200 400)
      (is (typep (setf layout (pango:cairo-create-layout cr)) 'pango:layout)))))

;;;     pango_cairo_update_layout

(test pango-cairo-update-layout
  (glib-test:with-check-memory (layout)
    (cairo:with-context-for-image-surface (cr :rgb24 200 400)
      (is (typep (setf layout (pango:cairo-create-layout cr)) 'pango:layout))
      (is-false (pango:cairo-update-layout cr layout)))))

;;;     pango_cairo_show_glyph_string

(test pango-cairo-show-glyph-string.1
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 3)
      (cairo:with-context-for-image-surface (cr :argb32 360 200)
        (let* ((text "Zwölf Ägypter auf der Straße")
               (context (pango:cairo-create-context cr))
               (attrstr "0 31 size 16384, 7 15 weight bold, 16 19 style italic")
               (attrs (pango:attr-list-from-string attrstr))
               (iter (pango:attr-list-iterator attrs))
               (path (glib-sys:sys-path "test/out/show-glyph-string.png"))
               items glyphs font)
          ;; Clear the background
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Set the color
          (cairo:set-source-rgb cr 0.5 0.5 0.5)
          ;; Move to the start position of the text
          (cairo:move-to cr 24 36)
          (setf items
                (pango:itemize context
                               text
                               0 (babel:string-size-in-octets text)
                               attrs
                               iter))
          (dolist (item items)
            (let ((str (babel:octets-to-string
                                  (subseq (babel:string-to-octets text)
                                          (pango:item-offset item)
                                          (+ (pango:item-offset item)
                                             (pango:item-length item))))))
            (when *verbose-pango-cairo-rendering*
              (format t "~&     item : ~a~%" item)
              (format t "   offset : ~a~%" (pango:item-offset item))
              (format t "   length : ~a~%" (pango:item-length item))
              (format t "      str : ~a~%" str))
            (setf glyphs
                  (pango:shape (babel:octets-to-string
                                  (subseq (babel:string-to-octets text)
                                          (pango:item-offset item)
                                          (+ (pango:item-offset item)
                                             (pango:item-length item))))
                               (pango:item-length item)
                               (pango:item-analysis item)))
            (setf font (pango:analysis-font (pango:item-analysis item)))
            ;; Print the text on the Cario context
            (pango:cairo-show-glyph-string cr font glyphs)
            (when *verbose-pango-cairo-rendering*
              (format t "    width : ~a~%" (/ (pango:glyph-string-width glyphs)
                                              pango:+scale+)))
            (cairo:rel-move-to cr
                               (/ (pango:glyph-string-width glyphs)
                                  pango:+scale+)
                               0)))
        ;; Create and save the PNG image
        (cairo:surface-write-to-png (cairo:target cr) path))))))

(test pango-cairo-show-glyph-string.2
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 3)
      (cairo:with-context-for-image-surface (cr :argb32 360 200)
        (let* ((text "Zwölf Ägypter auf der Straße")
               (context (pango:cairo-create-context cr))
               (attrstr "0 31 size 16384, 7 15 weight bold, 16 19 style italic")
               (attrs (pango:attr-list-from-string attrstr))
               (iter (pango:attr-list-iterator attrs))
               (path (glib-sys:sys-path "test/out/show-glyph-string2.png"))
               items glyphs font)
          ;; Clear the background
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Set the color
          (cairo:set-source-rgb cr 0.5 0.5 0.5)
          ;; Move to the start position of the text
          (cairo:move-to cr 24 36)
          (setf items
                (pango:itemize context
                               text
                               0 (babel:string-size-in-octets text)
                               attrs
                               iter))
          (dolist (item items)
            (let ((str (babel:octets-to-string
                                  (subseq (babel:string-to-octets text)
                                          (pango:item-offset item)
                                          (+ (pango:item-offset item)
                                             (pango:item-length item))))))
            (when *verbose-pango-cairo-rendering*
              (format t "~&     item : ~a~%" item)
              (format t "   offset : ~a~%" (pango:item-offset item))
              (format t "   length : ~a~%" (pango:item-length item))
              (format t "      str : ~a~%" str))
            (setf glyphs
                  (pango:shape-full str nil (pango:item-analysis item)))
            (setf font (pango:analysis-font (pango:item-analysis item)))
            ;; Print the text on the Cario context
            (pango:cairo-show-glyph-string cr font glyphs)
            (when *verbose-pango-cairo-rendering*
              (format t "    width : ~a~%" (/ (pango:glyph-string-width glyphs)
                                              pango:+scale+)))
            (cairo:rel-move-to cr
                               (/ (pango:glyph-string-width glyphs)
                                  pango:+scale+)
                               0)))
        ;; Create and save the PNG image
        (cairo:surface-write-to-png (cairo:target cr) path))))))

(test pango-cairo-show-glyph-string.3
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 3)
      (cairo:with-context-for-image-surface (cr :argb32 360 200)
        (let* ((text "Zwölf Ägypter auf der Straße")
               (context (pango:cairo-create-context cr))
               (attrstr "0 31 size 16384, 7 15 weight bold, 16 19 style italic")
               (attrs (pango:attr-list-from-string attrstr))
               (iter (pango:attr-list-iterator attrs))
               (path (glib-sys:sys-path "test/out/show-glyph-string3.png"))
               items glyphs font)
          ;; Clear the background
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Set the color
          (cairo:set-source-rgb cr 0.5 0.5 0.5)
          ;; Move to the start position of the text
          (cairo:move-to cr 24 36)
          (setf items
                (pango:itemize context
                               text
                               0 (babel:string-size-in-octets text)
                               attrs
                               iter))
          (dolist (item items)
            (let ((str (babel:octets-to-string
                                  (subseq (babel:string-to-octets text)
                                          (pango:item-offset item)
                                          (+ (pango:item-offset item)
                                             (pango:item-length item))))))
            (when *verbose-pango-cairo-rendering*
              (format t "~&     item : ~a~%" item)
              (format t "   offset : ~a~%" (pango:item-offset item))
              (format t "   length : ~a~%" (pango:item-length item))
              (format t "      str : ~a~%" str))
            (setf glyphs
                  (pango:shape-with-flags str
                                          nil
                                          (pango:item-analysis item)
                                          :round-positions))
            (setf font (pango:analysis-font (pango:item-analysis item)))
            ;; Print the text on the Cario context
            (pango:cairo-show-glyph-string cr font glyphs)
            (when *verbose-pango-cairo-rendering*
              (format t "    width : ~a~%" (/ (pango:glyph-string-width glyphs)
                                              pango:+scale+)))
            (cairo:rel-move-to cr
                               (/ (pango:glyph-string-width glyphs)
                                  pango:+scale+)
                               0)))
        ;; Create and save the PNG image
        (cairo:surface-write-to-png (cairo:target cr) path))))))

;;;     pango_cairo_show_glyph_item

(test pango-cairo-show-glyph-item
  (when *first-run-testsuite*
    (glib-test:with-check-memory (:strong 2)
      (cairo:with-context-for-image-surface (cr :argb32 360 200)
        (let* ((context (pango:cairo-create-context cr))
               (layout (pango:layout-new context))
               (text "Zwölf Ägypter auf der Straße")
               (iter nil)
               (path (glib-sys:sys-path "test/out/show-glyph-item.png"))
               glyphitem)
          (setf (pango:layout-text layout) text)
          (setf iter (pango:layout-iter layout))
          (setf glyphitem (pango:layout-iter-run iter))
          ;; Clear the background
          (cairo:set-source-rgb cr 1.0 1.0 1.0)
          (cairo:paint cr)
          ;; Set the color
          (cairo:set-source-rgb cr 0.5 0.5 0.5)
          ;; Set the font
          (setf (pango:layout-font-description layout)
                (pango:font-description-from-string "Courier Bold 16"))
          ;; Move to the start position of the text
          (cairo:move-to cr 24 24)
          ;; Print the text on the Cario context
          (pango:cairo-show-glyph-item cr text glyphitem)
          ;; Create and save the PNG image
          (cairo:surface-write-to-png (cairo:target cr) path))))))

;;;     pango_cairo_show_layout_line

;;;     pango_cairo_show_layout

(test pango-cairo-show-layout
  (glib-test:with-check-memory ()
    (cairo:with-context-for-image-surface (cr :argb32 360 200)
      (let ((layout (pango:cairo-create-layout cr)))
        ;; Clear the background
        (cairo:set-source-rgb cr 1.0 1.0 1.0)
        (cairo:paint cr)
        ;; Set the color
        (cairo:set-source-rgb cr 0.5 0.5 0.5)
        ;; Set the font
        (setf (pango:layout-font-description layout)
              (pango:font-description-from-string "Courier Bold 14"))
        ;; Set the text
        (setf (pango:layout-text layout) "Zwölf Ägypter auf der Straße")
        ;; Move to the start position of the text
        (cairo:move-to cr 24 24)
        ;; Print the text on the Cario context
        (pango:cairo-show-layout cr layout))
      ;; Create and save the PNG image
      (cairo:surface-write-to-png (cairo:target cr)
                                  (glib-sys:sys-path "test/out/show-layout.png")))))

;;;     pango_cairo_show_error_underline

;;;     pango_cairo_glyph_string_path
;;;     pango_cairo_layout_line_path
;;;     pango_cairo_layout_path
;;;     pango_cairo_error_underline_path

;;; 2025-09-17
