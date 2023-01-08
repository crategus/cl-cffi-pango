(in-package :pango-test)

(def-suite pango-attributes :in pango-suite)
(in-suite pango-attributes)

;;; --- Types and Values -------------------------------------------------------

;;;     PANGO_SCALE_XX_SMALL
;;;     PANGO_SCALE_X_SMALL
;;;     PANGO_SCALE_SMALL
;;;     PANGO_SCALE_MEDIUM
;;;     PANGO_SCALE_LARGE
;;;     PANGO_SCALE_X_LARGE
;;;     PANGO_SCALE_XX_LARGE

(test pango-scale-values
  (let ((eps 1.0d-6))
    (is (approx-equal (/ 1.0d0 (* 1.2d0 1.2d0 1.2d0)) +pango-scale-xx-small+ eps))
    (is (approx-equal (/ 1.0d0 (* 1.2d0 1.2d0)) +pango-scale-x-small+ eps))
    (is (approx-equal (/ 1.0d0 1.2d0) +pango-scale-small+ eps))
    (is (approx-equal 1.0d0 +pango-scale-medium+ eps))
    (is (approx-equal 1.2d0 +pango-scale-large+ eps))
    (is (approx-equal (* 1.2d0 1.2d0) +pango-scale-x-large+ eps))
    (is (approx-equal (* 1.2d0 1.2d0 1.2d0) +pango-scale-xx-large+ eps))))

;;;     PangoAttrType

(test pango-attr-type
  ;; Check the type
  (is (g:type-is-enum "PangoAttrType"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAttrType")
          (g:gtype (cffi:foreign-funcall "pango_attr_type_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:attr-type
          (gobject:symbol-for-gtype "PangoAttrType")))
  ;; Check the names
  (is (equal '("PANGO_ATTR_INVALID" "PANGO_ATTR_LANGUAGE" "PANGO_ATTR_FAMILY"
               "PANGO_ATTR_STYLE" "PANGO_ATTR_WEIGHT" "PANGO_ATTR_VARIANT"
               "PANGO_ATTR_STRETCH" "PANGO_ATTR_SIZE" "PANGO_ATTR_FONT_DESC"
               "PANGO_ATTR_FOREGROUND" "PANGO_ATTR_BACKGROUND"
               "PANGO_ATTR_UNDERLINE" "PANGO_ATTR_STRIKETHROUGH"
               "PANGO_ATTR_RISE" "PANGO_ATTR_SHAPE" "PANGO_ATTR_SCALE"
               "PANGO_ATTR_FALLBACK" "PANGO_ATTR_LETTER_SPACING"
               "PANGO_ATTR_UNDERLINE_COLOR" "PANGO_ATTR_STRIKETHROUGH_COLOR"
               "PANGO_ATTR_ABSOLUTE_SIZE" "PANGO_ATTR_GRAVITY"
               "PANGO_ATTR_GRAVITY_HINT" "PANGO_ATTR_FONT_FEATURES"
               "PANGO_ATTR_FOREGROUND_ALPHA" "PANGO_ATTR_BACKGROUND_ALPHA"
               "PANGO_ATTR_ALLOW_BREAKS" "PANGO_ATTR_SHOW"
               "PANGO_ATTR_INSERT_HYPHENS" "PANGO_ATTR_OVERLINE"
               "PANGO_ATTR_OVERLINE_COLOR" "PANGO_ATTR_LINE_HEIGHT"
               "PANGO_ATTR_ABSOLUTE_LINE_HEIGHT" "PANGO_ATTR_TEXT_TRANSFORM"
               "PANGO_ATTR_WORD" "PANGO_ATTR_SENTENCE"
               "PANGO_ATTR_BASELINE_SHIFT" "PANGO_ATTR_FONT_SCALE")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "PangoAttrType"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
               25 26 27 28 29 30 31 32 33 34 35 36 37)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "PangoAttrType"))))
  ;; Check the nick names
  (is (equal '("invalid" "language" "family" "style" "weight" "variant"
               "stretch" "size" "font-desc" "foreground" "background"
               "underline" "strikethrough" "rise" "shape" "scale" "fallback"
               "letter-spacing" "underline-color" "strikethrough-color"
               "absolute-size" "gravity" "gravity-hint" "font-features"
               "foreground-alpha" "background-alpha" "allow-breaks" "show"
               "insert-hyphens" "overline" "overline-color" "line-height"
               "absolute-line-height" "text-transform" "word" "sentence"
               "baseline-shift" "font-scale")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "PangoAttrType"))))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoAttrType"
                             PANGO-ATTR-TYPE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_attr_type_get_type")
                             (:INVALID 0)
                             (:LANGUAGE 1)
                             (:FAMILY 2)
                             (:STYLE 3)
                             (:WEIGHT 4)
                             (:VARIANT 5)
                             (:STRETCH 6)
                             (:SIZE 7)
                             (:FONT-DESC 8)
                             (:FOREGROUND 9)
                             (:BACKGROUND 10)
                             (:UNDERLINE 11)
                             (:STRIKETHROUGH 12)
                             (:RISE 13)
                             (:SHAPE 14)
                             (:SCALE 15)
                             (:FALLBACK 16)
                             (:LETTER-SPACING 17)
                             (:UNDERLINE-COLOR 18)
                             (:STRIKETHROUGH-COLOR 19)
                             (:ABSOLUTE-SIZE 20)
                             (:GRAVITY 21)
                             (:GRAVITY-HINT 22)
                             (:FONT-FEATURES 23)
                             (:FOREGROUND-ALPHA 24)
                             (:BACKGROUND-ALPHA 25)
                             (:ALLOW-BREAKS 26)
                             (:SHOW 27)
                             (:INSERT-HYPHENS 28)
                             (:OVERLINE 29)
                             (:OVERLINE-COLOR 30)
                             (:LINE-HEIGHT 31)
                             (:ABSOLUTE-LINE-HEIGHT 32)
                             (:TEXT-TRANSFORM 33)
                             (:WORD 34)
                             (:SENTENCE 35)
                             (:BASELINE-SHIFT 36)
                             (:FONT-SCALE 37))
             (gobject:get-g-type-definition "PangoAttrType"))))

;;;     PangoUnderline

(test underline
  ;; Check the type
  (is (g:type-is-enum "PangoUnderline"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoUnderline")
          (g:gtype (cffi:foreign-funcall "pango_underline_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:underline
          (gobject:symbol-for-gtype "PangoUnderline")))
  ;; Check the names
  (is (equal '("PANGO_UNDERLINE_NONE" "PANGO_UNDERLINE_SINGLE"
               "PANGO_UNDERLINE_DOUBLE" "PANGO_UNDERLINE_LOW"
               "PANGO_UNDERLINE_ERROR" "PANGO_UNDERLINE_SINGLE_LINE"
               "PANGO_UNDERLINE_DOUBLE_LINE" "PANGO_UNDERLINE_ERROR_LINE")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "PangoUnderline"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "PangoUnderline"))))
  ;; Check the nick names
  (is (equal '("none" "single" "double" "low" "error" "single-line"
               "double-line" "error-line")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "PangoUnderline"))))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoUnderline"
                             PANGO-UNDERLINE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_underline_get_type")
                             (:NONE 0)
                             (:SINGLE 1)
                             (:DOUBLE 2)
                             (:LOW 3)
                             (:ERROR 4)
                             (:SINGLE-LINE 5)
                             (:DOUBLE-LINE 6)
                             (:ERROR-LINE 7))
             (gobject:get-g-type-definition "PangoUnderline"))))

;;;     PangoOverline

(test overline
  ;; Check the type
  (is (g:type-is-enum "PangoOverline"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoOverline")
          (g:gtype (cffi:foreign-funcall "pango_overline_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:overline
          (gobject:symbol-for-gtype "PangoOverline")))
  ;; Check the names
  (is (equal '("PANGO_OVERLINE_NONE" "PANGO_OVERLINE_SINGLE")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "PangoOverline"))))
  ;; Check the values
  (is (equal '(0 1)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "PangoOverline"))))
  ;; Check the nick names
  (is (equal '("none" "single")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "PangoOverline"))))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoOverline"
                             PANGO-OVERLINE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_overline_get_type")
                             (:NONE 0)
                             (:SINGLE 1))
             (gobject:get-g-type-definition "PangoOverline"))))

;;;     PangoShowFlags

(test show-flags
  ;; Check the type
  (is (g:type-is-flags "PangoShowFlags"))
  ;; Check the registered name
  (is (eq 'pango:show-flags
          (gobject:symbol-for-gtype "PangoShowFlags")))
  ;; Check the type initializer
  (is (eq (gobject:gtype "PangoShowFlags")
          (gobject:gtype (cffi:foreign-funcall "pango_show_flags_get_type"
                                               :size))))
  ;; Check the names
  (is (equal '("PANGO_SHOW_NONE" "PANGO_SHOW_SPACES" "PANGO_SHOW_LINE_BREAKS"
               "PANGO_SHOW_IGNORABLES")
             (mapcar #'gobject:flags-item-name
                     (gobject:get-flags-items "PangoShowFlags"))))
  ;; Check the values
  (is (equal '(0 1 2 4)
             (mapcar #'gobject:flags-item-value
                     (gobject:get-flags-items "PangoShowFlags"))))
  ;; Check the nick names
  (is (equal '("none" "spaces" "line-breaks" "ignorables")
             (mapcar #'gobject:flags-item-nick
                     (gobject:get-flags-items "PangoShowFlags"))))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "PangoShowFlags"
                              PANGO-SHOW-FLAGS
                              (:EXPORT T
                               :TYPE-INITIALIZER "pango_show_flags_get_type")
                              (:NONE 0)
                              (:SPACES 1)
                              (:LINE-BREAKS 2)
                              (:IGNORABLES 4))
             (gobject:get-g-type-definition "PangoShowFlags"))))

;;;     PangoTextTransform                                 Since 1.50

(test text-transform
  ;; Check the type
  (is (g:type-is-enum "PangoTextTransform"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoTextTransform")
          (g:gtype (cffi:foreign-funcall "pango_text_transform_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:text-transform
          (gobject:symbol-for-gtype "PangoTextTransform")))
  ;; Check the names
  (is (equal '("PANGO_TEXT_TRANSFORM_NONE" "PANGO_TEXT_TRANSFORM_LOWERCASE"
               "PANGO_TEXT_TRANSFORM_UPPERCASE"
               "PANGO_TEXT_TRANSFORM_CAPITALIZE")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "PangoTextTransform"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "PangoTextTransform"))))
  ;; Check the nick names
  (is (equal '("none" "lowercase" "uppercase" "capitalize")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "PangoTextTransform"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoTextTransform"
                             PANGO-TEXT-TRANSFORM
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_text_transform_get_type")
                             (:NONE 0)
                             (:LOWERCASE 1)
                             (:UPPERCASE 2)
                             (:CAPITALIZE 3))
             (gobject:get-g-type-definition "PangoTextTransform"))))

;;;     PangoBaselineShift                                 Since 1.50

(test baseline-shift
  ;; Check the type
  (is (g:type-is-enum "PangoBaselineShift"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoBaselineShift")
          (g:gtype (cffi:foreign-funcall "pango_baseline_shift_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:baseline-shift
          (gobject:symbol-for-gtype "PangoBaselineShift")))
  ;; Check the names
  (is (equal '("PANGO_BASELINE_SHIFT_NONE" "PANGO_BASELINE_SHIFT_SUPERSCRIPT"
               "PANGO_BASELINE_SHIFT_SUBSCRIPT")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "PangoBaselineShift"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "PangoBaselineShift"))))
  ;; Check the nick names
  (is (equal '("none" "superscript" "subscript")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "PangoBaselineShift"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoBaselineShift"
                             PANGO-BASELINE-SHIFT
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_baseline_shift_get_type")
                             (:NONE 0)
                             (:SUPERSCRIPT 1)
                             (:SUBSCRIPT 2))
             (gobject:get-g-type-definition "PangoBaselineShift"))))

;;;     PangoFontScale                                     Since 1.50

(test font-scale
  ;; Check the type
  (is (g:type-is-enum "PangoFontScale"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoFontScale")
          (g:gtype (cffi:foreign-funcall "pango_font_scale_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:font-scale
          (gobject:symbol-for-gtype "PangoFontScale")))
  ;; Check the names
  (is (equal '("PANGO_FONT_SCALE_NONE" "PANGO_FONT_SCALE_SUPERSCRIPT"
               "PANGO_FONT_SCALE_SUBSCRIPT" "PANGO_FONT_SCALE_SMALL_CAPS")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "PangoFontScale"))))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "PangoFontScale"))))
  ;; Check the nick names
  (is (equal '("none" "superscript" "subscript" "small-caps")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "PangoFontScale"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoFontScale"
                             PANGO-FONT-SCALE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_font_scale_get_type")
                             (:NONE 0)
                             (:SUPERSCRIPT 1)
                             (:SUBSCRIPT 2)
                             (:SMALL-CAPS 3))
             (gobject:get-g-type-definition "PangoFontScale"))))

;;;     PangoAttrClass

;;;     PangoAttribute

(test attribute
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoAttribute") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAttribute")
          (g:gtype (cffi:foreign-funcall "pango_attribute_get_type" :size)))))

;;;     PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING
;;;     PANGO_ATTR_INDEX_TO_TEXT_END

;;;     PangoAttrString
;;;     PangoAttrLanguage
;;;     PangoAttrColor
;;;     PangoAttrInt
;;;     PangoAttrFloat
;;;     PangoAttrFontDesc
;;;     PangoAttrShape
;;;     PangoAttrSize
;;;     PangoAttrFontFeatures

;;;     PangoColor

(test color
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoColor") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoColor")
          (g:gtype (cffi:foreign-funcall "pango_color_get_type" :size)))))

;;;     PangoAttrList

(test attr-list
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoAttrList") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAttrList")
          (g:gtype (cffi:foreign-funcall "pango_attr_list_get_type" :size)))))

;;;     PangoAttrIterator

(test attr-iterator
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoAttrIterator") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAttrIterator")
          (g:gtype (cffi:foreign-funcall "pango_attr_iterator_get_type" :size)))))

;;; --- Functions --------------------------------------------------------------
;;;
;;;     pango_attr_type_register
;;;     pango_attr_type_get_name
;;;     pango_attribute_init
;;;     pango_attribute_copy
;;;     pango_attribute_equal
;;;     pango_attribute_destroy
;;;
;;;     pango_attr_language_new
;;;     pango_attr_family_new
;;;     pango_attr_style_new
;;;     pango_attr_variant_new
;;;     pango_attr_stretch_new
;;;     pango_attr_weight_new
;;;     pango_attr_size_new
;;;     pango_attr_size_new_absolute
;;;     pango_attr_font_desc_new
;;;     pango_attr_foreground_new
;;;     pango_attr_background_new
;;;     pango_attr_strikethrough_new
;;;     pango_attr_strikethrough_color_new
;;;     pango_attr_underline_new
;;;     pango_attr_underline_color_new
;;;     pango_attr_overline_new
;;;     pango_attr_overline_color_new
;;;
;;;     pango_attr_shape_new
;;;     pango_attr_shape_new_with_data
;;;
;;;     PangoAttrDataCopyFunc
;;;
;;;     pango_attr_scale_new
;;;     pango_attr_rise_new
;;;     pango_attr_letter_spacing_new
;;;     pango_attr_fallback_new
;;;     pango_attr_gravity_new
;;;     pango_attr_gravity_hint_new
;;;     pango_attr_font_features_new
;;;     pango_attr_foreground_alpha_new
;;;     pango_attr_background_alpha_new
;;;     pango_attr_allow_breaks_new
;;;     pango_attr_insert_hyphens_new
;;;     pango_attr_show_new
;;;
;;;     pango_color_parse
;;;     pango_color_parse_with_alpha
;;;     pango_color_copy
;;;     pango_color_free
;;;     pango_color_to_string
;;;
;;;     pango_attr_list_new
;;;     pango_attr_list_ref
;;;     pango_attr_list_unref
;;;     pango_attr_list_copy
;;;     pango_attr_list_insert
;;;     pango_attr_list_insert_before
;;;     pango_attr_list_change
;;;     pango_attr_list_splice
;;;     pango_attr_list_filter
;;;     pango_attr_list_update
;;;
;;;     PangoAttrFilterFunc
;;;
;;;     pango_attr_list_get_attributes

#+nil
(test pango-attr-list-attributes
  (let* ((label (make-instance 'gtk-label :text
                                          "<span foreground='blue' ~
                                                 size='x-large'> ~
                                           Blue text</span> is <i>cool</i>!"
                                          :use-markup t))
         (attributes (pango-layout-attributes (gtk-label-layout label))))

    (is (typep attributes 'pango-attr-list))
    (is (every (lambda (x) (typep x 'pango-attribute))
               (pango-attr-list-attributes attributes)))
))

;;;     pango_attr_list_equal
;;;     pango_attr_list_get_iterator
;;;     pango_attr_iterator_copy
;;;     pango_attr_iterator_next
;;;     pango_attr_iterator_range
;;;     pango_attr_iterator_get
;;;     pango_attr_iterator_get_font
;;;     pango_attr_iterator_get_attrs
;;;     pango_attr_iterator_destroy

;;; --- 2023-1-3 ---------------------------------------------------------------
