(in-package :pango-test)

(def-suite pango-attributes-suite :in pango-suite)
(in-suite pango-attributes-suite)

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
    (is (approx-equal (/ 1.0d0 (* 1.2d0 1.2d0 1.2d0)) pango:+scale-xx-small+ eps))
    (is (approx-equal (/ 1.0d0 (* 1.2d0 1.2d0)) pango:+scale-x-small+ eps))
    (is (approx-equal (/ 1.0d0 1.2d0) pango:+scale-small+ eps))
    (is (approx-equal 1.0d0 pango:+scale-medium+ eps))
    (is (approx-equal 1.2d0 pango:+scale-large+ eps))
    (is (approx-equal (* 1.2d0 1.2d0) pango:+scale-x-large+ eps))
    (is (approx-equal (* 1.2d0 1.2d0 1.2d0) pango:+scale-xx-large+ eps))))

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

(test pango-underline
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

(test pango-overline
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
             (list-enum-item-name "PangoOverline")))
  ;; Check the values
  (is (equal '(0 1)
             (list-enum-item-value "PangoOverline")))
  ;; Check the nick names
  (is (equal '("none" "single")
             (list-enum-item-nick "PangoOverline")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoOverline"
                             PANGO-OVERLINE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_overline_get_type")
                             (:NONE 0)
                             (:SINGLE 1))
             (gobject:get-g-type-definition "PangoOverline"))))

;;;     PangoShowFlags

(test pango-show-flags
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
             (list-flags-item-name "PangoShowFlags")))
  ;; Check the values
  (is (equal '(0 1 2 4)
             (list-flags-item-value "PangoShowFlags")))
  ;; Check the nick names
  (is (equal '("none" "spaces" "line-breaks" "ignorables")
             (list-flags-item-nick "PangoShowFlags")))
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

(test pango-text-transform
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
             (list-enum-item-name "PangoTextTransform")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "PangoTextTransform")))
  ;; Check the nick names
  (is (equal '("none" "lowercase" "uppercase" "capitalize")
             (list-enum-item-nick "PangoTextTransform")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoTextTransform"
                             PANGO-TEXT-TRANSFORM
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_text_transform_get_type")
                             (:NONE 0)
                             (:LOWERCASE 1)
                             (:UPPERCASE 2)
                             (:CAPITALIZE 3))
             (gobject:get-g-type-definition "PangoTextTransform"))))

;;;     PangoBaselineShift                                 Since 1.50

(test pango-baseline-shift
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
             (list-enum-item-name "PangoBaselineShift")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "PangoBaselineShift")))
  ;; Check the nick names
  (is (equal '("none" "superscript" "subscript")
             (list-enum-item-nick "PangoBaselineShift")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoBaselineShift"
                             PANGO-BASELINE-SHIFT
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_baseline_shift_get_type")
                             (:NONE 0)
                             (:SUPERSCRIPT 1)
                             (:SUBSCRIPT 2))
             (gobject:get-g-type-definition "PangoBaselineShift"))))

;;;     PangoFontScale                                     Since 1.50

(test pango-font-scale
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
             (list-enum-item-name "PangoFontScale")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "PangoFontScale")))
  ;; Check the nick names
  (is (equal '("none" "superscript" "subscript" "small-caps")
             (list-enum-item-nick "PangoFontScale")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoFontScale"
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

(test pango-attribute
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoAttribute") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAttribute")
          (g:gtype (cffi:foreign-funcall "pango_attribute_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:attribute
          (glib:symbol-for-gtype "PangoAttribute"))))

(test pango-attribut-slots
  (let ((attr (pango:attr-language-new (pango:language-default))))
    (is (eq :language (pango:attribute-type attr)))
    (is (= 10 (setf (pango:attribute-start-index attr) 10)))
    (is (= 10 (pango:attribute-start-index attr)))
    (is (= 20 (setf (pango:attribute-end-index attr) 20)))
    (is (= 20 (pango:attribute-end-index attr)))))

;;;     pango_attribute_init

;;;     pango_attribute_copy
;;;     pango_attribute_equal

(test pango-attribute-copy/equal
  (let* ((attr1 (pango::attr-size-new 99))
         (attr2 (pango:attribute-copy attr1))
         (attr3 (pango::attr-language-new (pango:language-default))))
    (is-true (pango:attribute-equal attr1 attr2))
    (is-false (pango:attribute-equal attr1 attr3))
    (is-false (pango:attribute-equal attr2 attr3))))

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

(test pango-attr-list
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoAttrList") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAttrList")
          (g:gtype (cffi:foreign-funcall "pango_attr_list_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:attr-list
          (glib:symbol-for-gtype "PangoAttrList"))))

;;;     PangoAttrIterator

(test pango-attr-iterator
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoAttrIterator") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAttrIterator")
          (g:gtype (cffi:foreign-funcall "pango_attr_iterator_get_type"
                                         :size))))
  ;; Check the registered name
  (is (eq 'pango:attr-iterator
          (glib:symbol-for-gtype "PangoAttrIterator"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_attr_type_register
;;;     pango_attr_type_get_name

(test pango-attr-type-register/name
  (let (id1 id2)
    (is (integerp (setf id1 (pango:attr-type-register "newtype1"))))
    (is (string= "newtype1" (pango:attr-type-name id1)))
    (is (integerp (setf id2 (pango:attr-type-register "newtype2"))))
    (is (string= "newtype2" (pango:attr-type-name id2)))
    (is (not (= id1 id2)))))

;;;     pango_attr_language_new

(test pango-attr-language-new
  (let ((attr (pango:attr-language-new (pango:language-default)))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :language (pango:attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))
    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 language de-de"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_family_new

(test pango-attr-familiy-new
  (let ((attr (pango:attr-family-new "Sans"))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :family (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))
    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 family \"Sans\""
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_style_new

(test pango-attr-style-new
  (let ((attr (pango:attr-style-new :normal))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :style (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 style normal"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_variant_new

(test pango-attr-variant-new
  (let ((attr (pango:attr-variant-new :normal))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :variant (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 variant normal"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_stretch_new

(test pango-attr-stretch-new
  (let ((attr (pango:attr-stretch-new :normal))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :stretch (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 stretch normal"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_weight_new

(test pango-attr-weight-new
  (let ((attr (pango:attr-weight-new :bold))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :weight (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 weight bold"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_size_new

(test pango-attr-size-new
  (let ((attr (pango:attr-size-new 16))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :size (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 size 16"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_size_new_absolute

(test pango-attr-size-new-absolute
  (let ((attr (pango:attr-size-new-absolute 16))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :absolute-size (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 absolute-size 16"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_font_desc_new

(test pango-attr-font-desc-new
  (let ((attr (pango:attr-font-desc-new (pango:font-description-new)))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :font-desc (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 font-desc \"Normal\""
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_foreground_new

(test pango-attr-foreground-new
  (let ((attr (pango:attr-foreground-new 10 20 30))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :foreground (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 foreground #000a0014001e"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_background_new

(test pango-attr-background-new
  (let ((attr (pango:attr-background-new 10 20 30))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :background (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 background #000a0014001e"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_strikethrough_new

(test pango-attr-strikethrough-new
  (let ((attr (pango:attr-strikethrough-new t))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :strikethrough (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 strikethrough true"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_strikethrough_color_new

(test pango-attr-strikethrough-color-new
  (let ((attr (pango:attr-strikethrough-color-new 10 20 30))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :strikethrough-color (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 strikethrough-color #000a0014001e"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_underline_new

(test pango-attr-underline-new
  (let ((attr (pango:attr-underline-new :double))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :underline (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 underline double"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_underline_color_new

(test pango-attr-underline-color-new
  (let ((attr (pango:attr-underline-color-new 10 20 30))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :underline-color (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 underline-color #000a0014001e"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_overline_new

(test pango-attr-overline-new
  (let ((attr (pango:attr-overline-new :single))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :overline (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 overline single"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_overline_color_new

(test pango-attr-overline-color-new
  (let ((attr (pango:attr-overline-color-new 10 20 30))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :overline-color (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 overline-color #000a0014001e"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_shape_new

;; TODO: Consider to implement pango:rectangle more lispy!?

(test pango-attr-shape-new
  (cffi:with-foreign-objects ((rect1 '(:struct pango:rectangle))
                              (rect2 '(:struct pango:rectangle)))
    (cffi:with-foreign-slots ((pango::x pango::y pango::width pango::height)
                              rect1 (:struct pango:rectangle))
      (setf pango::x 10 pango::y 20 pango::width 30 pango::height 40))
    (cffi:with-foreign-slots ((pango::x pango::y pango::width pango::height)
                              rect2 (:struct pango:rectangle))
      (setf pango::x 50 pango::y 60 pango::width 70 pango::height 80))

    (let ((attr (pango:attr-shape-new rect1 rect2))
          (attrlist (pango:attr-list-new)))
      (is (typep attr 'pango:attribute))
      (is (eq :shape (pango::attribute-type attr)))
      (is (= 0 (pango:attribute-start-index attr)))
      (is (= 4294967295 (pango:attribute-end-index attr)))

      (is-false (pango:attr-list-insert attrlist attr))
      (is (string= "0 4294967295 shapeshape"
                   (pango:attr-list-to-string attrlist))))))

;;;     PangoAttrDataCopyFunc
;;;     pango_attr_shape_new_with_data

;;;     pango_attr_scale_new

(test pango-attr-scale-new
  (let ((attr (pango:attr-scale-new 2.0d0))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :scale (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 scale 2.000000"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_rise_new

(test pango-attr-rise-new
  (let ((attr (pango:attr-rise-new 2))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :rise (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 rise 2"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_letter_spacing_new

(test pango-attr-letter-spacing-new
  (let ((attr (pango:attr-letter-spacing-new 2))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :letter-spacing (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 letter-spacing 2"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_fallback_new

(test pango-attr-fallback-new
  (let ((attr (pango:attr-fallback-new t))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :fallback (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 fallback true"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_gravity_new

(test pango-attr-gravity-new
  (let ((attr (pango:attr-gravity-new :north))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :gravity (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 gravity north"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_gravity_hint_new

(test pango-attr-gravity-hint-new
  (let ((attr (pango:attr-gravity-hint-new :natural))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :gravity-hint (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 gravity-hint natural"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_font_features_new

(test pango-attr-font-features-new
  (let ((attr (pango:attr-font-features-new "smcp"))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :font-features (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 font-features \"smcp\""
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_foreground_alpha_new

(test pango-attr-foreground-alpha-new
  (let ((attr (pango:attr-foreground-alpha-new 32))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :foreground-alpha (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 foreground-alpha 32"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_background_alpha_new

(test pango-attr-background-alpha-new
  (let ((attr (pango:attr-background-alpha-new 32))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :background-alpha (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 background-alpha 32"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_allow_breaks_new

(test pango-attr-allow-breaks-new
  (let ((attr (pango:attr-allow-breaks-new t))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :allow-breaks (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 allow-breaks true"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_insert_hyphens_new

(test pango-attr-insert-hyphens-new
  (let ((attr (pango:attr-insert-hyphens-new t))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :insert-hyphens (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 insert-hyphens true"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_show_new

(test pango-attr-show-new
  (let ((attr (pango:attr-show-new :spaces))
        (attrlist (pango:attr-list-new)))
    (is (typep attr 'pango:attribute))
    (is (eq :show (pango::attribute-type attr)))
    (is (= 0 (pango:attribute-start-index attr)))
    (is (= 4294967295 (pango:attribute-end-index attr)))

    (is-false (pango:attr-list-insert attrlist attr))
    (is (string= "0 4294967295 show 1"
                 (pango:attr-list-to-string attrlist)))))

;;;     PangoAttrFilterFunc

;;;     pango_attr_list_new

(test pango-attr-list-new
  (let ((attrs (pango:attr-list-new)))
    (is (typep attrs 'pango:attr-list))))

;;;     pango_attr_list_ref
;;;     pango_attr_list_unref

;;;     pango_attr_list_copy
;;;     pango_attr_list_equal

(test pango-attr-list-copy/equal
  (let* ((attrlist1 (pango:attr-list-from-string "5 15 weight bold"))
         (attrlist2 (pango:attr-list-copy attrlist1)))
    (is (pango:attr-list-equal attrlist1 attrlist2))
    (is-false (pango:attr-list-copy nil))))

;;;     pango_attr_list_insert

(test pango-attr-list-insert
  (let ((attrlist (pango:attr-list-new)))
    (is-false (pango:attr-list-insert attrlist
                                      (pango:attr-weight-new :bold)))
    (is (string= "0 4294967295 weight bold"
                 (pango:attr-list-to-string attrlist)))
    (is-false (pango:attr-list-insert attrlist
                                      (pango:attr-size-new 16)))
    (is (string= "0 4294967295 weight bold
0 4294967295 size 16"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_list_insert_before

(test pango-attr-list-insert-before
  (let ((attrlist (pango:attr-list-new)))
    (is-false (pango:attr-list-insert-before attrlist
                                             (pango:attr-weight-new :bold)))
    (is (string= "0 4294967295 weight bold"
                 (pango:attr-list-to-string attrlist)))
    (is-false (pango:attr-list-insert-before attrlist
                                             (pango:attr-size-new 16)))
    (is (string= "0 4294967295 size 16
0 4294967295 weight bold"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_list_change

(test pango-attr-list-change
  (let ((attrlist (pango:attr-list-from-string " 5 15 weight bold,
                                                20 25 weight bold,
                                                30 35 weight bold")))
    (is (string= "5 15 weight bold
20 25 weight bold
30 35 weight bold"
                 (pango:attr-list-to-string attrlist)))

    (is-false (pango:attr-list-change attrlist (pango:attr-weight-new :bold)))

    (is (string= "0 4294967295 weight bold"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_list_splice
;;;     pango_attr_list_filter
;;;     pango_attr_list_update

;;;     pango_attr_list_get_attributes

(test pango-attr-list-attributes
  (let ((attrlist (pango:attr-list-from-string " 5 15 weight bold,
                                                20 25 size 99,
                                                30 35 underline single")))
    (is (equal '(:weight :size :underline)
               (mapcar #'pango:attribute-type
                       (pango:attr-list-attributes attrlist))))))

;;;     pango_attr_list_from_string
;;;     pango_attr_list_to_string

(test pango-attr-list-from-string/to-string
  (let (attrlist)
    (is (typep (setf attrlist
                     (pango:attr-list-from-string "5 15 weight bold"))
               'pango:attr-list))
    (is (string= "5 15 weight bold"
                 (pango:attr-list-to-string attrlist)))))

;;;     pango_attr_list_get_iterator

(test pango-attr-list-iterator
  (let ((attrlist (pango:attr-list-from-string " 5 15 weight bold,
                                                20 25 size 99")))
    (is (typep (pango:attr-list-iterator attrlist) 'pango:attr-iterator))))

;;;     pango_attr_iterator_copy

(test pango-attr-iterator-copy
  (let* ((attrlist (pango:attr-list-from-string " 5 15 weight bold,
                                                 20 25 size 99"))
         (iterator (pango:attr-list-iterator attrlist)))
    (is (typep (pango:attr-iterator-copy iterator) 'pango:attr-iterator))))

;;;     pango_attr_iterator_get
;;;     pango_attr_iterator_next

(test pango-attr-iterator-get/next.1
  (let* ((attrlist (pango:attr-list-from-string " 0 3 weight bold,
                                                  0 5 size 99
                                                  0 7 underline single"))
         (iterator (pango:attr-list-iterator attrlist)))

    (is (equal '(:weight :size :underline)
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))
    (is (eq :weight
            (pango:attribute-type (pango:attr-iterator-get iterator :weight))))
    (is (eq :size
            (pango:attribute-type (pango:attr-iterator-get iterator :size))))
    (is (eq :underline
            (pango:attribute-type (pango:attr-iterator-get iterator :underline))))

    (is-true (pango:attr-iterator-next iterator))
    (is (equal '(:size :underline)
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))
    (is-false (pango:attr-iterator-get iterator :weight))
    (is (eq :size
            (pango:attribute-type (pango:attr-iterator-get iterator :size))))
    (is (eq :underline
            (pango:attribute-type (pango:attr-iterator-get iterator :underline))))

    (is-true (pango:attr-iterator-next iterator))
    (is (equal '(:underline)
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))
    (is-false (pango:attr-iterator-get iterator :weight))
    (is-false (pango:attr-iterator-get iterator :size))
    (is (eq :underline
            (pango:attribute-type (pango:attr-iterator-get iterator :underline))))

    (is-true (pango:attr-iterator-next iterator))
    (is (equal '()
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))
    (is-false (pango:attr-iterator-get iterator :weight))
    (is-false (pango:attr-iterator-get iterator :size))
    (is-false (pango:attr-iterator-get iterator :underline))

    (is-false (pango:attr-iterator-next iterator))))

(test pango-attr-iterator-get/next.2
  (let* ((attrlist (pango:attr-list-from-string " 0 3 weight bold,
                                                  5 7 size 99
                                                  9 11 underline single"))
         (iterator (pango:attr-list-iterator attrlist)))

    (is (equal '(:weight)
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))

    (is-true (pango:attr-iterator-next iterator))
    (is (equal '()
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))

    (is-true (pango:attr-iterator-next iterator))
    (is (equal '(:size)
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))

    (is-true (pango:attr-iterator-next iterator))
    (is (equal '()
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))

    (is-true (pango:attr-iterator-next iterator))
    (is (equal '(:underline)
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))

    (is-true (pango:attr-iterator-next iterator))
    (is (equal '()
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))
    (is-false (pango:attr-iterator-next iterator))))

;;;     pango_attr_iterator_range

(test pango-attr-iterator-range
  (let* ((attrlist (pango:attr-list-from-string " 0 3 lang de-de
                                                  0 5 weight bold,
                                                  0 7 size 99
                                                  0 9 underline single"))
         (iterator (pango:attr-list-iterator attrlist)))
    (is (equal '(0 3)
               (multiple-value-list (pango:attr-iterator-range iterator))))
    (is-true (pango:attr-iterator-next iterator))
    (is (equal '(3 5)
               (multiple-value-list (pango:attr-iterator-range iterator))))
    (is-true (pango:attr-iterator-next iterator))
    (is (equal '(5 7)
               (multiple-value-list (pango:attr-iterator-range iterator))))
    (is-true (pango:attr-iterator-next iterator))
    (is (equal '(7 9)
               (multiple-value-list (pango:attr-iterator-range iterator))))
    (is-true (pango:attr-iterator-next iterator))
    (is (equal '(9 2147483647)
               (multiple-value-list (pango:attr-iterator-range iterator))))
    (is-false (pango:attr-iterator-next iterator))))

;;;     pango_attr_iterator_get_font

(test pango-attr-iterator-font
  (let* ((attrlist (pango:attr-list-from-string " 0 3 lang de-de
                                                  0 3 weight bold,
                                                  0 3 size 99
                                                  0 3 underline single"))
         (iterator (pango:attr-list-iterator attrlist)))
    (multiple-value-bind (desc language attrs)
        (pango:attr-iterator-font iterator)
      (is (typep desc 'pango:font-description))
      (is (string= "de-de" (pango:language-to-string language)))
      (is (equal '(:underline)
                 (mapcar #'pango:attribute-type attrs))))))

;;;     pango_attr_iterator_get_attrs

(test pango-attr-iterator-attrs
  (let* ((attrlist (pango:attr-list-from-string " 0 3 weight bold,
                                                  0 3 size 99
                                                  0 3 underline single"))
         (iterator (pango:attr-list-iterator attrlist)))
    (is (equal '(:weight :size :underline)
               (mapcar #'pango:attribute-type
                       (pango:attr-iterator-attrs iterator))))))

;;;     pango_attr_iterator_destroy

;;; 2024-3-1
