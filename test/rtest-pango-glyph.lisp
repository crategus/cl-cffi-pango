(in-package :pango-test)

(def-suite pango-glyph-suite :in pango-suite)
(in-suite pango-glyph-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoGlyph
;;;     PANGO_GLYPH_EMPTY
;;;     PANGO_GLYPH_INVALID_INPUT
;;;     PANGO_GLYPH_UNKNOWN_FLAG
;;;     PangoGlyphInfo
;;;     PangoGlyphGeometry
;;;     PangoGlyphUnit
;;;     PangoGlyphVisAttr

;;;     PangoGlyphString

(test pango-glyph-string-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoGlyphString"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoGlyphString")
          (g:gtype (cffi:foreign-funcall "pango_glyph_string_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:glyph-string
          (glib:symbol-for-gtype "PangoGlyphString"))))

;;;     PangoGlyphItem

(test pango-glyph-item-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoGlyphItem"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoGlyphItem")
          (g:gtype (cffi:foreign-funcall "pango_glyph_item_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:glyph-item
          (glib:symbol-for-gtype "PangoGlyphItem"))))

;;;     PangoGlyphItemIter
;;;     PANGO_TYPE_GLYPH_STRING

;;; --- Functions --------------------------------------------------------------

;;;     PANGO_GET_UNKNOWN_GLYPH

;;;     pango_glyph_string_new

(test pango-glyph-string-new
  (is (typep (pango:glyph-string-new) 'pango:glyph-string)))

;;;     pango_glyph_string_copy

(test pango-glyph-string-copy
  (is (typep (pango:glyph-string-copy (pango:glyph-string-new))
             'pango:glyph-string)))

;;;     pango_glyph_string_set_size

(test pango-glyph-string-set-size
  (let* ((text "Zwölf Ägypter gehen über die Straße.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (items (pango:itemize context
                               text
                               0
                               (babel:string-size-in-octets text)
                               nil
                               nil))
         (item (first items))
         (glyphs (pango:shape text (pango:item-analysis item))))

    (is (= 290390 (pango:glyph-string-width glyphs)))
    (is-false (pango:glyph-string-set-size glyphs 20))
    (is (= 166102 (pango:glyph-string-width glyphs)))))

;;;     pango_glyph_string_free

;;;     pango_glyph_string_extents
;;;     pango_glyph_string_get_width

(test pango-glyph-string-extents
  (let* ((text "Zwölf Ägypter gehen über die Straße.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         (items (pango:itemize context
                               text
                               0
                               (babel:string-size-in-octets text)
                               nil
                               nil))
         (item (first items))
         (glyphs (pango:shape text (pango:item-analysis item))))

    (is (typep glyphs 'pango:glyph-string))

    (is (= 290390 (pango:glyph-string-width glyphs)))
    (is (= 40 (babel:string-size-in-octets text)))
    (is (= (pango:item-length item) (babel:string-size-in-octets text)))

    (pango:with-rectangles (ink logical)
      ;; Get extents
      (is-false (pango:glyph-string-extents glyphs font ink logical))
      ;; Check ink extents
      (is (=      0 (pango:rectangle-x ink)))
      (is (= -15360 (pango:rectangle-y ink)))
      (is (= 290390 (pango:rectangle-width ink)))
      (is (=  19456 (pango:rectangle-height ink)))
      ;; Check logical extent
      (is (=      0 (pango:rectangle-x logical)))
      (is (= -18432 (pango:rectangle-y logical)))
      (is (= 290390 (pango:rectangle-width logical)))
      (is (=  23552 (pango:rectangle-height logical))))))

;;;     pango_glyph_string_extents_range

(test pango-glyph-string-extents-range
  (let* ((text "Zwölf Ägypter gehen über die Straße.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (font (pango:font-map-load-font fontmap context desc))
         (items (pango:itemize context
                               text
                               0
                               (babel:string-size-in-octets text)
                               nil
                               nil))
         (item (first items))
         (glyphs (pango:shape text (pango:item-analysis item))))

    (is (typep glyphs 'pango:glyph-string))

    (is (= 290390 (pango:glyph-string-width glyphs)))
    (is (= 40 (babel:string-size-in-octets text)))
    (is (= (pango:item-length item) (babel:string-size-in-octets text)))

    (pango:with-rectangles (ink logical)
      ;; Get extents with range
      (is-false (pango:glyph-string-extents-range glyphs 0 2 font ink logical))
      ;; Check ink extents
      (is (=      0 (pango:rectangle-x ink)))
      (is (= -12288 (pango:rectangle-y ink)))
      (is (=  23011 (pango:rectangle-width ink)))
      (is (=  12288 (pango:rectangle-height ink)))
      ;; Check logical extent
      (is (=      0 (pango:rectangle-x logical)))
      (is (= -18432 (pango:rectangle-y logical)))
      (is (=  23822 (pango:rectangle-width logical)))
      (is (=  23552 (pango:rectangle-height logical))))))

;;;     pango_glyph_string_index_to_x
;;;     pango_glyph_string_x_to_index

(test pango-glyph-string-index-to-x/x-to-index
  (let* ((text "Zwölf Ägypter gehen über die Straße.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (items (pango:itemize context
                               text
                               0
                               (babel:string-size-in-octets text)
                               nil
                               nil))
         (item (first items))
         (glyphs (pango:shape text (pango:item-analysis item)))
         listxpos1 listxpos2)

    (is (typep glyphs 'pango:glyph-string))

    (is (= 290390 (pango:glyph-string-width glyphs)))
    (is (= 40 (babel:string-size-in-octets text)))
    (is (= (pango:item-length item) (babel:string-size-in-octets text)))

    (is (equal '(     0   9699  23822  33276  33276  38355  44401  48661  59884
                  59884  68699  77956  88016  93783 102548 110265 114525 123340
                 132105 142509 151274 161842 166102 176506 176506 186566 195331
                 203048 207308 217368 222611 231376 235636 244549 250316 257705
                 266929 277529 277529 286294)
               (setf listxpos1
                     (iter (for index from 0 below (pango:item-length item))
                           (collect (pango:glyph-string-index-to-x
                                            glyphs
                                            text
                                            (pango:item-length item)
                                            (pango:item-analysis item)
                                            index
                                            nil))))))

    (is (equal '(  9699  23822  33276  33276  38355  44401  48661  59884  59884
                  68699  77956  88016  93783 102548 110265 114525 123340 132105
                 142509 151274 161842 166102 176506 176506 186566 195331 203048
                 207308 217368 222611 231376 235636 244549 250316 257705 266929
                 277529 277529 286294 290390)
               (setf listxpos2
                     (iter (for index from 0 below (pango:item-length item))
                           (collect (pango:glyph-string-index-to-x
                                            glyphs
                                            text
                                            (pango:item-length item)
                                            (pango:item-analysis item)
                                            index
                                            t))))))

    (is (equal '( 0  1  2  4  4  5  6  7  9  9 10 11 12 13 14 15 16 17 18 19 20
                 21 22 24 24 25 26 27 28 29 30 31 32 33 34 35 36 38 38 39)
        (iter (for xpos in listxpos1)
              (collect (pango:glyph-string-x-to-index
                               glyphs
                               text
                               (pango:item-length item)
                               (pango:item-analysis item)
                               xpos)))))

    (is (equal '( 1  2  4  4  5  6  7  9  9 10 11 12 13 14 15 16 17 18 19 20 21
                 22 24 24 25 26 27 28 29 30 31 32 33 34 35 36 38 38 39 40)
        (iter (for xpos in listxpos2)
              (format t "~a : ~a~%" xpos
                                    (pango:glyph-string-x-to-index
                                            glyphs
                                            text
                                            (pango:item-length item)
                                            (pango:item-analysis item)
                                            xpos))
              (collect (pango:glyph-string-x-to-index
                               glyphs
                               text
                               (pango:item-length item)
                               (pango:item-analysis item)
                               xpos)))))))

;;;     pango_glyph_string_get_logical_widths

(test pango-glyph-string-logical-widths
  (let* ((text "Zwölf Ägypter gehen über die Straße.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (items (pango:itemize context
                               text
                               0
                               (babel:string-size-in-octets text)
                               nil
                               nil))
         (item (first items))
         (glyphs (pango:shape text (pango:item-analysis item))))

    (is (equalp ' #1a( 9699 14123 9454  5079 6046  4260 11223 8815 9257 10060
                       5767  8765 7717  4260 8815  8765 10404 8765 10568 4260
                      10404 10060 8765  7717 4260 10060  5243 8765 4260  8913
                       5767  7389 9224 10600 8765  4096)
                (pango:glyph-string-logical-widths glyphs
                                                   text
                                                   (pango:item-length item)
                                                   0)))))

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

;;; 2026-03-08
