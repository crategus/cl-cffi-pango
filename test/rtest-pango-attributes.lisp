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
          (glib:symbol-for-gtype "PangoAttrType")))
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
             (list-enum-item-name "PangoAttrType")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
               25 26 27 28 29 30 31 32 33 34 35 36 37)
             (list-enum-item-value "PangoAttrType")))
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
             (list-enum-item-nick "PangoAttrType")))
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
          (glib:symbol-for-gtype "PangoUnderline")))
  ;; Check the names
  (is (equal '("PANGO_UNDERLINE_NONE" "PANGO_UNDERLINE_SINGLE"
               "PANGO_UNDERLINE_DOUBLE" "PANGO_UNDERLINE_LOW"
               "PANGO_UNDERLINE_ERROR" "PANGO_UNDERLINE_SINGLE_LINE"
               "PANGO_UNDERLINE_DOUBLE_LINE" "PANGO_UNDERLINE_ERROR_LINE")
             (list-enum-item-name "PangoUnderline")))
  ;; Check the values
  (is (equal '(0 1 2 3 4 5 6 7)
             (list-enum-item-value "PangoUnderline")))
  ;; Check the nick names
  (is (equal '("none" "single" "double" "low" "error" "single-line"
               "double-line" "error-line")
             (list-enum-item-nick "PangoUnderline")))
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
          (glib:symbol-for-gtype "PangoOverline")))
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
          (glib:symbol-for-gtype "PangoShowFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoShowFlags")
          (g:gtype (cffi:foreign-funcall "pango_show_flags_get_type" :size))))
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
          (g:gtype (cffi:foreign-funcall "pango_text_transform_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'pango:text-transform
          (glib:symbol-for-gtype "PangoTextTransform")))
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
          (g:gtype (cffi:foreign-funcall "pango_baseline_shift_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'pango:baseline-shift
          (glib:symbol-for-gtype "PangoBaselineShift")))
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
          (glib:symbol-for-gtype "PangoFontScale")))
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

;;;     PangoColor

(test color
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoColor") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoColor")
          (g:gtype (cffi:foreign-funcall "pango_color_get_type" :size)))))

;;;     color-new
;;;     color-copy
;;;     color-red
;;;     color-green
;;;     color-blue

(test color-new/copy
  (let ((color (pango:color-new :red 1 :green 2 :blue 3)))
    (is (typep color 'pango:color))
    (is (= 1 (pango:color-red color)))
    (is (= 2 (pango:color-green color)))
    (is (= 3 (pango:color-blue color)))
    (is (typep (pango:color-copy color) 'pango:color))
    (is (= 4 (setf (pango:color-red color) 4)))
    (is (= 4 (pango:color-red (pango:color-copy color))))
    (is (= 5 (setf (pango:color-green color) 5)))
    (is (= 5 (pango:color-green (pango:color-copy color))))
    (is (= 6 (setf (pango:color-blue color) 6)))
    (is (= 6 (pango:color-blue (pango:color-copy color))))))

;;;     pango_color_parse
;;;     pango_color_to_string

(test color-parse/to-string
  (let ((color (pango:color-parse "red")))
    (is (= 65535 (pango:color-red color)))
    (is (= 0 (pango:color-green color)))
    (is (= 0 (pango:color-blue color)))
    (is (string= "#ffff00000000" (pango:color-to-string color)))))

;;;     pango_color_parse_with_alpha

(test color-parse-with-alpha
  (multiple-value-bind (color alpha)
      (pango:color-parse-with-alpha "red")
    (is (string= "#ffff00000000" (pango:color-to-string color)))
    (is (= 65535 alpha))))

;;;     PangoAttrClass

;;;     PangoAttribute

(test attribute
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoAttribute") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAttribute")
          (g:gtype (cffi:foreign-funcall "pango_attribute_get_type" :size)))))

(test attribute-new
  (let ((attr (pango:attribute-new)))
    (is-false (pango:attribute-klass attr))
    (is-false (pango:attribute-start-index attr))
    (is-false (pango:attribute-end-index attr))))

(test attribute-slots
  (let ((attr (pango:attr-size-new 99)))
    (is (cffi:pointerp (pango:attribute-klass attr)))
    (is (eq :size (pango:attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))))

;;;     pango_attribute_init

;;;     pango_attribute_copy
;;;     pango_attribute_equal

;; TODO: Does not work as expected. There is something wrong.

(test attribute-copy/equal
  (let* ((attr1 (pango:attr-size-new 99))
         (attr2 (pango:attribute-copy attr1)))

    (is (eq :size (pango:attribute-type attr1)))
    (is (= 0 (pango:attribute-start-index attr1)))
    (is (= 4294967295 (pango:attribute-end-index attr1)))

    (is (eq :size (pango:attribute-type attr2)))
    (is (= 0 (pango:attribute-start-index attr2)))
    (is (= 4294967295 (pango:attribute-end-index attr2)))
    ;; attr1 is not equal attr2?! Is this correct?
    ;; TODO: Is sometimes TRUE and sometimes FALSE
;    (is-true (pango:attribute-equal attr1 attr2))
))

;;;     pango_attribute_destroy

;;;     PangoAttrString
;;;     PangoAttrLanguage
;;;     PangoAttrColor
;;;     PangoAttrInt
;;;     PangoAttrFloat
;;;     PangoAttrFontDesc
;;;     PangoAttrShape
;;;     PangoAttrSize
;;;     PangoAttrFontFeatures

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

;;;     pango_attr_type_register
;;;     pango_attr_type_get_name

(test attr-type-register
  (let (id1 id2)
    (is (integerp (setf id1 (pango:attr-type-register "newtype1"))))
    (is (string= "newtype1" (pango:attr-type-name id1)))
    (is (integerp (setf id2 (pango:attr-type-register "newtype2"))))
    (is (string= "newtype2" (pango:attr-type-name id2)))
    (is (not (= id1 id2)))))

;;;     pango_attr_language_new

(test attr-language-new
  (let ((attr (pango:attr-language-new (pango:language-default))))
    (is (typep attr 'pango:attribute))
    (is (eq :language (pango:attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))))

;;;     pango_attr_family_new

(test attr-familiy-new
  (let ((attr (pango:attr-family-new "Sans")))
    (is (typep attr 'pango:attribute))
    (is (eq :family (pango:attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))))

;;;     pango_attr_style_new

(test attr-style-new
  (let ((attr (pango:attr-style-new :normal)))
    (is (typep attr 'pango:attribute))
    (is (eq :style (pango:attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))))

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

;;;     pango_attr_list_new

(test attr-list-new
  (let ((attrs (pango:attr-list-new)))
    (is (typep attrs 'pango:attr-list))))

;;;     pango_attr_list_ref
;;;     pango_attr_list_unref
;;;     pango_attr_list_copy

;;;     pango_attr_list_insert

;; TODO: Inseration of an attribute does not work as expected.

(test attr-list-insert
  (let ((attrs (pango:attr-list-new)))

    (is (typep attrs 'pango:attr-list))

    (is-false (pango:attr-list-insert attrs (pango:attr-size-new 99)))
    (is (string= "" (pango:attr-list-to-string (pango:attr-list-new))))
;    (is-false (pango:attr-list-attributes attrs))
))


;;;     pango_attr_list_insert_before

;;;     pango_attr_list_change

;; TODO: Inseration of an attribute does not work as expected. What is wrong?

;; --------------------------------
;;  ATTR-LIST-CHANGE in PANGO-ATTRIBUTES []:
;;       Unexpected Error: #<SB-SYS:MEMORY-FAULT-ERROR {100433AF53}>
;; Unhandled memory fault at #x10..
;; --------------------------------

#+nil
(test attr-list-change
  (trace pango:attr-list-change)
  (trace pango:attr-list-to-string)
  (trace pango:attr-list-new)
  (let ((attrs (pango:attr-list-new))
        (attr (pango:attr-size-new 99)))
    (is (typep attrs 'pango:attr-list))
    (is (string= "" (pango:attr-list-to-string attrs)))
    (is-false (pango:attr-list-change attrs attr))
    ;; TODO: The following test generates the memory fault.
    (is-false (pango:attr-list-to-string attrs))
;    (is-false (pango:attr-list-attributes attrs))
)
  (untrace pango:attr-list-change)
  (untrace pango:attr-list-to-string)
  (untrace pango:attr-list-new))

;;;     pango_attr_list_splice
;;;     pango_attr_list_filter
;;;     pango_attr_list_update
;;;
;;;     PangoAttrFilterFunc
;;;
;;;     pango_attr_list_get_attributes
;;;     pango_attr_list_equal

;;;     pango_attr_list_from_string
;;;     pango_attr_list_to_string

;;;     pango_attr_list_get_iterator

#+nil
(test attr-list-iterator
  (let ((attrs (pango:attr-list-new)))

    (is (typep (pango:attr-list-iterator attrs) 'pango:attr-iterator))
    (is-false (pango:attr-list-insert attrs
                                      (pango:attr-family-new "Sans")))
    (is (typep (pango:attr-list-iterator attrs) 'pango:attr-iterator))))

;;;     pango_attr_iterator_copy
;;;     pango_attr_iterator_next
;;;     pango_attr_iterator_range
;;;     pango_attr_iterator_get
;;;     pango_attr_iterator_get_font
;;;     pango_attr_iterator_get_attrs
;;;     pango_attr_iterator_destroy

;;; --- 2023-5-29 --------------------------------------------------------------
