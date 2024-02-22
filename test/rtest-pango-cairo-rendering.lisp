(in-package :pango-test)

(def-suite pango-cairo-rendering :in pango-suite)
(in-suite pango-cairo-rendering)

;;; -- Types and Values --------------------------------------------------------

;;;     PangoCairoFont

(test pango-cairo-font-interface
  ;; Type check
  (is (g:type-is-interface "PangoCairoFont"))
  ;; Check the registered name
  (is (eq 'pango:cairo-font
          (glib:symbol-for-gtype "PangoCairoFont")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoCairoFont")
          (g:gtype (cffi:foreign-funcall "pango_cairo_font_get_type" :size))))
  ;; Check the interface properties
  (is (equal '()
             (list-interface-properties "PangoCairoFont")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "PangoCairoFont"
                                  PANGO-CAIRO-FONT
                                  (:EXPORT T
                                   :TYPE-INITIALIZER
                                   "pango_cairo_font_get_type"))
             (gobject:get-g-type-definition "PangoCairoFont"))))

;;;     PangoCairoFontMap

;; FIXME: We get an error when getting the GType definition.

;;  CAIRO-FONT-MAP-INTERFACE in PANGO-CAIRO-RENDERING []:
;;       Unexpected Error: #<SB-SYS:MEMORY-FAULT-ERROR {1003972BA3}>
;; Unhandled memory fault at #x0..

(test pango-cairo-font-map-interface
  ;; Type check
  (is (g:type-is-interface "PangoCairoFontMap"))
  ;; Check the registered name
  (is (eq 'pango:cairo-font-map
          (glib:symbol-for-gtype "PangoCairoFontMap")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoCairoFontMap")
          (g:gtype (cffi:foreign-funcall "pango_cairo_font_map_get_type"
                                         :size))))
  ;; Check the interface properties
  (is (equal '()
             (list-interface-properties "PangoCairoFontMap")))
  ;; Get the interface definition
  (is (equal '(GOBJECT:DEFINE-G-INTERFACE "PangoCairoFontMap"
                                  PANGO-CAIRO-FONT-MAP
                                  (:EXPORT T
                                   :TYPE-INITIALIZER
                                   "pango_cairo_font_map_get_type"))
             (gobject:get-g-type-definition "PangoCairoFontMap"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_cairo_font_map_get_default
;;;     pango_cairo_font_map_set_default

(test pango-cairo-font-map-default
  (is (typep (pango:cairo-font-map-default) 'pango:font-map))
  (is (typep (setf (pango:cairo-font-map-default) (pango:cairo-font-map-new))
             'pango:font-map))
  (is (typep (pango:cairo-font-map-default) 'pango:font-map)))

;;;     pango_cairo_font_map_new

(test pango-cairo-font-map-new
  (is (typep (pango:cairo-font-map-new) 'pango:font-map)))

;;;     pango_cairo_font_map_new_for_font_type

(test pango-cairo-font-map-new-for-font-type
  (is-false (pango:cairo-font-map-new-for-font-type :toy))
  (is-true  (pango:cairo-font-map-new-for-font-type :ft))
  #-windows
  (is-false (pango:cairo-font-map-new-for-font-type :win32))
  #+windows
  (is-true  (pango:cairo-font-map-new-for-font-type :win32))
  (is-false (pango:cairo-font-map-new-for-font-type :quartz))
  (is-false (pango:cairo-font-map-new-for-font-type :user))

  (is (typep (pango:cairo-font-map-new-for-font-type :ft) 'pango:font-map)))

;;;     pango_cairo_font_map_get_font_type

#-windows
(test pango-cairo-font-map-font-type
  (is (eq :ft (pango:cairo-font-map-font-type (pango:cairo-font-map-default))))
  (is (eq :ft (pango:cairo-font-map-font-type (pango:cairo-font-map-new)))))

#+windows
(test pango-cairo-font-map-font-type
  (is (eq :win32
          (pango:cairo-font-map-font-type (pango:cairo-font-map-default))))
  (is (eq :win32 (pango:cairo-font-map-font-type (pango:cairo-font-map-new)))))

;;;     pango_cairo_font_map_set_resolution
;;;     pango_cairo_font_map_get_resolution

(test pango-cairo-font-map-resolution
  (let ((fontmap (pango:cairo-font-map-default)))
    (is (=  96.0d0 (pango:cairo-font-map-resolution fontmap)))
    (is (= 200.0d0 (setf (pango:cairo-font-map-resolution fontmap) 200)))
    (is (= 200.0d0 (pango:cairo-font-map-resolution fontmap)))
    ;; Restore the default resolution
    (is (=  96.0d0 (setf (pango:cairo-font-map-resolution fontmap) 96.0d0)))))

;;;     pango_cairo_font_map_create_context                not exported

;;;     pango_cairo_font_get_scaled_font

;; TODO: The argument is a pango:font object, but how do we get it from a
;; pango:font-map?

;;;     pango_cairo_context_set_resolution
;;;     pango_cairo_context_get_resolution

(test pango-cairo-context-resolution
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default))))
    (is (= -1.0d0 (pango:cairo-context-resolution context)))
    (is (= 96.0d0 (setf (pango:cairo-context-resolution context) 96)))
    (is (= 96.0d0 (pango:cairo-context-resolution context)))))

;;;     pango_cairo_context_set_font_options
;;;     pango_cairo_context_get_font_options

(test pango-cairo-context-font-options
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default)))
        (options (cairo:font-options-create)))
    (is-false (pango:cairo-context-font-options context))
    ;; Set the font options
    (is (cffi:pointer-eq options
                         (setf (pango:cairo-context-font-options context)
                               options)))
    (is (cffi:pointerp (pango:cairo-context-font-options context)))
    ;; Unset the font options
    (is-false (setf (pango:cairo-context-font-options context) nil))
    (is-false (pango:cairo-context-font-options context))
    (is-false (cairo:font-options-destroy options))))

;;;     PangoCairoShapeRendererFunc                        not exported
;;;     pango_cairo_context_set_shape_renderer             not exported
;;;     pango_cairo_context_get_shape_renderer             not exported

;;;     pango_cairo_create_context

(test pango-cairo-create-context
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (is (typep (pango:cairo-create-context cr) 'pango:context))))

;;;     pango_cairo_update_context

(test pango-cairo-update-context
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let ((context (pango:cairo-create-context cr)))
      (is (typep context 'pango:context))
      (is-false (pango:cairo-update-context cr context)))))

;;;     pango_cairo_create_layout

(test pango-cairo-create-layout
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (is (typep (pango:cairo-create-layout cr) 'pango:layout))))

;;;     pango_cairo_update_layout

(test pango-cairo-update-layout
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let ((layout (pango:cairo-create-layout cr)))
      (is (typep layout 'pango:layout))
      (is-false (pango:cairo-update-layout cr layout)))))

;;;     pango_cairo_show_glyph_string
;;;     pango_cairo_show_glyph_item
;;;     pango_cairo_show_layout_line
;;;     pango_cairo_show_layout
;;;     pango_cairo_show_error_underline
;;;     pango_cairo_glyph_string_path
;;;     pango_cairo_layout_line_path
;;;     pango_cairo_layout_path
;;;     pango_cairo_error_underline_path

;;; --- 2023-7-18 --------------------------------------------------------------
