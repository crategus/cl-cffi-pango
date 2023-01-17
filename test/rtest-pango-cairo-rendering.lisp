(in-package :pango-test)

(def-suite pango-cairo-rendering :in pango-suite)
(in-suite pango-cairo-rendering)

;;; -- Types and Values --------------------------------------------------------

;;;     PangoCairoFont

(test cairo-font-interface
  ;; Type check
  (is (g:type-is-interface "PangoCairoFont"))
  ;; Check the registered name
  (is (eq 'pango:cairo-font
          (gobject:symbol-for-gtype "PangoCairoFont")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoCairoFont")
          (g:gtype (cffi:foreign-funcall "pango_cairo_font_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "PangoCairoFont")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "PangoCairoFont"
                                  PANGO-CAIRO-FONT
                                  (:EXPORT T
                                   :TYPE-INITIALIZER
                                   "pango_cairo_font_get_type"))
             (gobject:get-g-type-definition "PangoCairoFont"))))

;;;     PangoCairoFontMap

(test cairo-font-map-interface
  ;; Type check
  (is (g:type-is-interface "PangoCairoFontMap"))
  ;; Check the registered name
  (is (eq 'pango:cairo-font-map
          (gobject:symbol-for-gtype "PangoCairoFontMap")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoCairoFontMap")
          (g:gtype (cffi:foreign-funcall "pango_cairo_font_map_get_type" :size))))
  ;; Get the names of the interface properties.
  (is (equal '()
             (list-interface-properties "PangoCairoFontMap")))
  ;; Get the interface definition
  (is (equal '(DEFINE-G-INTERFACE "PangoCairoFontMap"
                                  PANGO-CAIRO-FONT-MAP
                                  (:EXPORT T
                                   :TYPE-INITIALIZER
                                   "pango_cairo_font_map_get_type"))
             (gobject:get-g-type-definition "PangoCairoFontMap"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_cairo_font_map_get_default
;;;     pango_cairo_font_map_set_default

(test cairo-font-map-default
  (is (typep (pango:cairo-font-map-default) 'pango:font-map))
  (is (typep (setf (pango:cairo-font-map-default) (pango:cairo-font-map-new))
             'pango:font-map))
  (is (typep (pango:cairo-font-map-default) 'pango:font-map)))

;;;     pango_cairo_font_map_new

(test cairo-font-map-new
  (is (typep (pango:cairo-font-map-new) 'pango:font-map)))

;;;     pango_cairo_font_map_new_for_font_type

(test cairo-font-map-new-for-font-type
  (is-false (pango:cairo-font-map-new-for-font-type :toy))
  (is-true  (pango:cairo-font-map-new-for-font-type :ft))
  (is-false (pango:cairo-font-map-new-for-font-type :win32))
  (is-false (pango:cairo-font-map-new-for-font-type :quartz))
  (is-false (pango:cairo-font-map-new-for-font-type :user))

  (is (typep (pango:cairo-font-map-new-for-font-type :ft) 'pango:font-map)))

;;;     pango_cairo_font_map_get_font_type

(test cairo-font-map-font-type
  (is (eq :ft (pango:cairo-font-map-font-type (pango:cairo-font-map-default))))
  (is (eq :ft (pango:cairo-font-map-font-type (pango:cairo-font-map-new))))
  (is (eq :ft (pango:cairo-font-map-font-type (pango:cairo-font-map-default)))))

;;;     pango_cairo_font_map_set_resolution
;;;     pango_cairo_font_map_get_resolution

(test cairo-font-map-resolution
  (let ((fontmap (pango:cairo-font-map-default)))
    (is (=  96.0d0 (pango:cairo-font-map-resolution fontmap)))
    (is (= 200.0d0 (setf (pango:cairo-font-map-resolution fontmap) 200)))
    (is (= 200.0d0 (pango:cairo-font-map-resolution fontmap)))))

;;;     pango_cairo_font_map_create_context

;; TODO: Gets a pango:font-map object, but expects an pango:cairo-font-map
;; object.

#+nil
(test cairo-font-map-create-context
  (let ((fontmap (pango:cairo-font-map-default)))
    (is-false (pango:cairo-font-map-create-context fontmap))))

;;;     pango_cairo_font_get_scaled_font

;; TODO: The argument is a pango:font object, but how do we get it from a
;; pango:font-map?

;;;     pango_cairo_context_set_resolution
;;;     pango_cairo_context_get_resolution

(test cairo-context-resolution
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default))))
    (is (= -1.0d0 (pango:cairo-context-resolution context)))
    (is (= 96.0d0 (setf (pango:cairo-context-resolution context) 96)))
    (is (= 96.0d0 (pango:cairo-context-resolution context)))))

;;;     pango_cairo_context_set_font_options
;;;     pango_cairo_context_get_font_options

(test cairo-context-font-options
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default)))
        (options (cairo:font-options-create)))
    (is-false (pango:cairo-context-font-options context))
    (is (cffi:pointer-eq options
                         (setf (pango:cairo-context-font-options context)
                               options)))
    ;; TODO: The pointers are no longer eq! Why?
    #+nil
    (is (cffi:pointer-eq options
                         (pango:cairo-context-font-options context)))
    (is-false (cairo:font-options-destroy options))))

;;;     PangoCairoShapeRendererFunc
;;;     pango_cairo_context_set_shape_renderer
;;;     pango_cairo_context_get_shape_renderer

;;;     pango_cairo_create_context

(test cairo-create-context
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (is (typep (pango:cairo-create-context cr) 'pango:context))))

;;;     pango_cairo_update_context

(test cairo-update-context
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let ((context (pango:cairo-create-context cr)))
      (is (typep context 'pango:context))
      (is-false (pango:cairo-update-context cr context)))))

;;;     pango_cairo_create_layout

(test cairo-create-layout
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (is (typep (pango:cairo-create-layout cr) 'pango:layout))))

;;;     pango_cairo_update_layout

(test cairo-update-layout
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
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

;;; --- 2023-1-17 --------------------------------------------------------------
