(in-package :pango-test)

(def-suite pango-glyph :in pango-suite)
(in-suite pango-glyph)

;;; --- Types and Values -------------------------------------------------------
;;;
;;;     PANGO_SCALE
;;;     PangoRectangle
;;;     PangoGlyph
;;;     PANGO_GLYPH_EMPTY
;;;     PANGO_GLYPH_INVALID_INPUT
;;;     PANGO_GLYPH_UNKNOWN_FLAG
;;;     PangoGlyphInfo
;;;     PangoGlyphGeometry
;;;     PangoGlyphUnit
;;;     PangoGlyphVisAttr
;;;     PangoGlyphString
;;;     PangoGlyphItem
;;;     PangoGlyphItemIter
;;;     PANGO_TYPE_GLYPH_STRING

;;; --- Functions --------------------------------------------------------------

;;;     PANGO_PIXELS
;;;     PANGO_PIXELS_FLOOR
;;;     PANGO_PIXELS_CEIL
;;;     PANGO_UNITS_ROUND

;;;     pango_units_to_double
;;;     pango_units_from_double

;;;     PANGO_ASCENT
;;;     PANGO_DESCENT
;;;     PANGO_LBEARING
;;;     PANGO_RBEARING

;;;     pango_extents_to_pixels

;;;     PangoMatrix

(test pango-matrix-cstruct
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoMatrix") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoMatrix")
          (g:gtype (cffi:foreign-funcall "pango_matrix_get_type" :size)))))

;;;     PANGO_MATRIX_INIT
;;;     pango:matrix-new
;;;     pango_matrix_copy

#+nil
(test pango-matrix-new/copy/init
  (let* ((matrix1 (pango:matrix-init))
         (matrix2 (pango:matrix-new :xx 1 :yy 2))
         (matrix3 (pango:matrix-copy matrix2)))

    (is-false (pango:matrix-xx matrix1))
    (is-false (pango:matrix-xy matrix1))
    (is-false (pango:matrix-yx matrix1))
    (is-false (pango:matrix-yy matrix1))
    (is-false (pango:matrix-x0 matrix1))
    (is-false (pango:matrix-y0 matrix1))
))

;;;     pango_matrix_translate
;;;     pango_matrix_scale
;;;     pango_matrix_rotate
;;;     pango_matrix_concat
;;;     pango_matrix_transform_point
;;;     pango_matrix_transform_distance
;;;     pango_matrix_transform_rectangle
;;;     pango_matrix_transform_pixel_rectangle
;;;     pango_matrix_get_font_scale_factor
;;;     pango_matrix_get_font_scale_factors

;;;     PANGO_GET_UNKNOWN_GLYPH

;;;     pango_glyph_string_new
;;;     pango_glyph_string_copy
;;;     pango_glyph_string_set_size
;;;     pango_glyph_string_free
;;;     pango_glyph_string_extents
;;;     pango_glyph_string_extents_range
;;;     pango_glyph_string_get_width
;;;     pango_glyph_string_index_to_x
;;;     pango_glyph_string_x_to_index
;;;     pango_glyph_string_get_logical_widths
;;;
;;;     pango_glyph_item_copy
;;;     pango_glyph_item_free
;;;     pango_glyph_item_split
;;;     pango_glyph_item_apply_attrs
;;;     pango_glyph_item_letter_space
;;;     pango_glyph_item_get_logical_widths
;;;
;;;     pango_glyph_item_iter_copy
;;;     pango_glyph_item_iter_free
;;;     pango_glyph_item_iter_init_start
;;;     pango_glyph_item_iter_init_end
;;;     pango_glyph_item_iter_next_cluster
;;;     pango_glyph_item_iter_prev_cluster

;;; --- 2023-7-17 --------------------------------------------------------------
