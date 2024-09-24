(in-package :pango-test)

(def-suite pango-font-description-suite :in pango-suite)
(in-suite pango-font-description-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoStyle

(test pango-style
  ;; Check type
  (is (g:type-is-enum "PangoStyle"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoStyle")
          (g:gtype (cffi:foreign-funcall "pango_style_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:style
          (glib:symbol-for-gtype "PangoStyle")))
  ;; Check names
  (is (equal '("PANGO_STYLE_NORMAL" "PANGO_STYLE_OBLIQUE" "PANGO_STYLE_ITALIC")
             (glib-test:list-enum-item-names "PangoStyle")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "PangoStyle")))
  ;; Check nick names
  (is (equal '("normal" "oblique" "italic")
             (glib-test:list-enum-item-nicks "PangoStyle")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoStyle" PANGO:STYLE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "pango_style_get_type")
                                    (:NORMAL 0)
                                    (:OBLIQUE 1)
                                    (:ITALIC 2))
             (gobject:get-gtype-definition "PangoStyle"))))

;;;     PangoWeight

(test pango-weight
  ;; Check type
  (is (g:type-is-enum "PangoWeight"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoWeight")
          (g:gtype (cffi:foreign-funcall "pango_weight_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:weight
          (glib:symbol-for-gtype "PangoWeight")))
  ;; Check names
  (is (equal '("PANGO_WEIGHT_THIN" "PANGO_WEIGHT_ULTRALIGHT"
               "PANGO_WEIGHT_LIGHT" "PANGO_WEIGHT_SEMILIGHT" "PANGO_WEIGHT_BOOK"
               "PANGO_WEIGHT_NORMAL" "PANGO_WEIGHT_MEDIUM"
               "PANGO_WEIGHT_SEMIBOLD" "PANGO_WEIGHT_BOLD"
               "PANGO_WEIGHT_ULTRABOLD" "PANGO_WEIGHT_HEAVY"
               "PANGO_WEIGHT_ULTRAHEAVY")
             (glib-test:list-enum-item-names "PangoWeight")))
  ;; Check values
  (is (equal '(100 200 300 350 380 400 500 600 700 800 900 1000)
             (glib-test:list-enum-item-values "PangoWeight")))
  ;; Check nick names
  (is (equal '("thin" "ultralight" "light" "semilight" "book" "normal" "medium"
               "semibold" "bold" "ultrabold" "heavy" "ultraheavy")
             (glib-test:list-enum-item-nicks "PangoWeight")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoWeight" PANGO:WEIGHT
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "pango_weight_get_type")
                                    (:THIN 100)
                                    (:ULTRALIGHT 200)
                                    (:LIGHT 300)
                                    (:SEMILIGHT 350)
                                    (:BOOK 380)
                                    (:NORMAL 400)
                                    (:MEDIUM 500)
                                    (:SEMIBOLD 600)
                                    (:BOLD 700)
                                    (:ULTRABOLD 800)
                                    (:HEAVY 900)
                                    (:ULTRAHEAVY 1000))
             (gobject:get-gtype-definition "PangoWeight"))))

;;;     PangoVariant

(test pango-variant
  ;; Check type
  (is (g:type-is-enum "PangoVariant"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoVariant")
          (g:gtype (cffi:foreign-funcall "pango_variant_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:variant
          (glib:symbol-for-gtype "PangoVariant")))
  ;; Check names
  (is (equal '("PANGO_VARIANT_NORMAL" "PANGO_VARIANT_SMALL_CAPS"
               "PANGO_VARIANT_ALL_SMALL_CAPS" "PANGO_VARIANT_PETITE_CAPS"
               "PANGO_VARIANT_ALL_PETITE_CAPS" "PANGO_VARIANT_UNICASE"
               "PANGO_VARIANT_TITLE_CAPS")
             (glib-test:list-enum-item-names "PangoVariant")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "PangoVariant")))
  ;; Check nick names
  (is (equal '("normal" "small-caps" "all-small-caps" "petite-caps"
               "all-petite-caps" "unicase" "title-caps")
             (glib-test:list-enum-item-nicks "PangoVariant")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoVariant" PANGO:VARIANT
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "pango_variant_get_type")
                                    (:NORMAL 0)
                                    (:SMALL-CAPS 1)
                                    (:ALL-SMALL-CAPS 2)
                                    (:PETITE-CAPS 3)
                                    (:ALL-PETITE-CAPS 4)
                                    (:UNICASE 5)
                                    (:TITLE-CAPS 6))
             (gobject:get-gtype-definition "PangoVariant"))))

;;;     PangoStretch

(test pango-stretch
  ;; Check type
  (is (g:type-is-enum "PangoStretch"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoStretch")
          (g:gtype (cffi:foreign-funcall "pango_stretch_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:stretch
          (glib:symbol-for-gtype "PangoStretch")))
  ;; Check names
  (is (equal '("PANGO_STRETCH_ULTRA_CONDENSED" "PANGO_STRETCH_EXTRA_CONDENSED"
               "PANGO_STRETCH_CONDENSED" "PANGO_STRETCH_SEMI_CONDENSED"
               "PANGO_STRETCH_NORMAL" "PANGO_STRETCH_SEMI_EXPANDED"
               "PANGO_STRETCH_EXPANDED" "PANGO_STRETCH_EXTRA_EXPANDED"
               "PANGO_STRETCH_ULTRA_EXPANDED")
             (glib-test:list-enum-item-names "PangoStretch")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6 7 8)
             (glib-test:list-enum-item-values "PangoStretch")))
  ;; Check nick names
  (is (equal '("ultra-condensed" "extra-condensed" "condensed" "semi-condensed"
               "normal" "semi-expanded" "expanded" "extra-expanded"
               "ultra-expanded")
             (glib-test:list-enum-item-nicks "PangoStretch")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoStretch" PANGO:STRETCH
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "pango_stretch_get_type")
                                    (:ULTRA-CONDENSED 0)
                                    (:EXTRA-CONDENSED 1)
                                    (:CONDENSED 2)
                                    (:SEMI-CONDENSED 3)
                                    (:NORMAL 4)
                                    (:SEMI-EXPANDED 5)
                                    (:EXPANDED 6)
                                    (:EXTRA-EXPANDED 7)
                                    (:ULTRA-EXPANDED 8))
             (gobject:get-gtype-definition "PangoStretch"))))

;;;     PangoFontMask

(test pango-font-mask
  ;; Check type
  (is (g:type-is-flags "PangoFontMask"))
  ;; Check registered name
  (is (eq 'pango:font-mask
          (glib:symbol-for-gtype "PangoFontMask")))
  ;; Check type initializer
  (is (eq (g:gtype "PangoFontMask")
          (g:gtype (cffi:foreign-funcall "pango_font_mask_get_type" :size))))
  ;; Check names
  (is (equal '("PANGO_FONT_MASK_FAMILY" "PANGO_FONT_MASK_STYLE"
               "PANGO_FONT_MASK_VARIANT" "PANGO_FONT_MASK_WEIGHT"
               "PANGO_FONT_MASK_STRETCH" "PANGO_FONT_MASK_SIZE"
               "PANGO_FONT_MASK_GRAVITY" "PANGO_FONT_MASK_VARIATIONS")
             (glib-test:list-flags-item-names "PangoFontMask")))
  ;; Check values
  (is (equal '(1 2 4 8 16 32 64 128)
             (glib-test:list-flags-item-values "PangoFontMask")))
  ;; Check nick names
  (is (equal '("family" "style" "variant" "weight" "stretch" "size" "gravity"
               "variations")
             (glib-test:list-flags-item-nicks "PangoFontMask")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "PangoFontMask" PANGO:FONT-MASK
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "pango_font_mask_get_type")
                                     (:FAMILY 1)
                                     (:STYLE 2)
                                     (:VARIANT 4)
                                     (:WEIGHT 8)
                                     (:STRETCH 16)
                                     (:SIZE 32)
                                     (:GRAVITY 64)
                                     (:VARIATIONS 128))
             (gobject:get-gtype-definition "PangoFontMask"))))

;;;     PangoFontDescription

(test pango-font-description-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoFontDescription"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoFontDescription")
          (g:gtype (cffi:foreign-funcall "pango_font_description_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'pango:font-description
          (glib:symbol-for-gtype "PangoFontDescription"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_font_description_new

(test pango-font-description-new
  (is (typep (pango:font-description-new) 'pango:font-description)))

;;;     pango_font_description_copy

(test pango-font-description-copy
  (let ((desc (pango:font-description-new)))
    (is (typep (pango:font-description-copy desc) 'pango:font-description))))

;;;     pango_font_description_hash

(test pango-font-description-hash
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (integerp (pango:font-description-hash desc)))))

;;;     pango_font_description_equal

(test pango-font-description-equal
  (let ((desc1 (pango:font-description-from-string "Sans Bold 16"))
        (desc2 (pango:font-description-from-string "Sans Bold 18"))
        (desc3 (pango:font-description-from-string "Sans Bold 18")))
    (is-false (pango:font-description-equal desc1 desc2))
    (is-true (pango:font-description-equal desc1 desc1))
    (is-true (pango:font-description-equal desc2 desc2))
    (is-false (pango:font-description-equal desc1 desc3))
    (is-true (pango:font-description-equal desc2 desc3))))

;;;     pango_font_description_free
;;;     pango_font_descriptions_free

;;;     pango_font_description_set_family
;;;     pango_font_description_get_family

(test pango-font-description-family
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (string= "Sans" (pango:font-description-family desc)))
    (is (string= "Verdana"
                 (setf (pango:font-description-family desc) "Verdana")))
    (is (string= "Verdana Bold 16"
                 (pango:font-description-to-string desc)))))

;;;     pango_font_description_set_style
;;;     pango_font_description_get_style

(test pango-font-description-style
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (eq :normal (pango:font-description-style desc)))
    (is (eq :italic (setf (pango:font-description-style desc) :italic)))
    (is (eq :italic (pango:font-description-style desc)))))

;;;     pango_font_description_set_variant
;;;     pango_font_description_get_variant

(test pango-font-description-variant
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (eq :normal (pango:font-description-variant desc)))
    (is (eq :small-caps
            (setf (pango:font-description-variant desc) :small-caps)))
    (is (eq :small-caps (pango:font-description-variant desc)))))

;;;     pango_font_description_set_weight
;;;     pango_font_description_get_weight

(test pango-font-description-weight.1
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (eq :bold (pango:font-description-weight desc)))
    (is (eq :thin (setf (pango:font-description-weight desc) :thin)))
    (is (eq :thin (pango:font-description-weight desc)))))

;; The PangoWeight enumeration allows undeclared values.

(test pango-font-description-weight.2
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (eq :bold (pango:font-description-weight desc)))
    (is (= 450 (setf (pango:font-description-weight desc) 450)))
    (is (= 450 (pango:font-description-weight desc)))
    (is (= 400 (setf (pango:font-description-weight desc) 400)))
    (is (eq :normal (pango:font-description-weight desc)))))

;;;     pango_font_description_set_stretch
;;;     pango_font_description_get_stretch

(test pango-font-description-stretch
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (eq :normal (pango:font-description-stretch desc)))
    (is (eq :expanded (setf (pango:font-description-stretch desc) :expanded)))
    (is (eq :expanded (pango:font-description-stretch desc)))))

;;;     pango_font_description_set_size
;;;     pango_font_description_get_size

(test pango-font-description-size
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (= (* 16 pango:+scale+) (pango:font-description-size desc)))
    (is (= (* 18 pango:+scale+)
           (setf (pango:font-description-size desc)
                 (* 18 pango:+scale+))))
    (is (= (* 18 pango:+scale+)
           (pango:font-description-size desc)))))

;;;     pango_font_description_set_absolute_size
;;;     pango_font_description_get_size_is_absolute

(test pango-font-description-absolute-size
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is-false (pango:font-description-size-is-absolute desc))
    (is (= (* 16 pango:+scale+) (pango:font-description-size desc)))
    (is-false (pango:font-description-set-absolute-size desc 18))
    (is-true (pango:font-description-size-is-absolute desc))
    (is (= 18 (pango:font-description-size desc)))))

;;;     pango_font_description_set_gravity
;;;     pango_font_description_get_gravity

(test font-description-gravity
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (eq :south (pango:font-description-gravity desc)))
    (is (eq :north (setf (pango:font-description-gravity desc) :north)))
    (is (eq :north (pango:font-description-gravity desc)))))

;;;     pango_font_description_set_variations
;;;     pango_font_description_get_variations

;; no example for a font with variations

(test pango-font-description-variations
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is-false (pango:font-description-variations desc))))

;;;     pango_font_description_get_set_fields
;;;     pango_font_description_unset_fields

(test pango-font-description-set-fields
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (equal '(:FAMILY :STYLE :VARIANT :WEIGHT :STRETCH :SIZE)
               (pango:font-description-set-fields desc)))
    (is-false (pango:font-description-unset-fields desc :family))
    (is (equal '(:STYLE :VARIANT :WEIGHT :STRETCH :SIZE)
               (pango:font-description-set-fields desc)))
    (is-false (pango:font-description-unset-fields desc '(:style :variant)))
    (is (equal '(:WEIGHT :STRETCH :SIZE)
               (pango:font-description-set-fields desc)))))

;;;     pango_font_description_merge

(test pango-font-description-merge.1
  (let ((desc1 (pango:font-description-from-string "16"))
        (desc2 (pango:font-description-from-string "Sans Bold")))
    (is (string= "Normal 16" (pango:font-description-to-string desc1)))
    (is (string= "Sans Bold" (pango:font-description-to-string desc2)))
    (is-false (pango:font-description-merge desc1 desc2 t))
    (is (string= "Sans Bold 16" (pango:font-description-to-string desc1)))))

(test pango-font-description-merge.2
  (let ((desc1 (pango:font-description-from-string "16"))
        (desc2 (pango:font-description-from-string "Sans Bold")))
    (is (string= "Normal 16" (pango:font-description-to-string desc1)))
    (is (string= "Sans Bold" (pango:font-description-to-string desc2)))
    (is-false (pango:font-description-merge desc1 nil t))
    (is (string= "Normal 16" (pango:font-description-to-string desc1)))))

;;;     pango_font_description_better_match

;;;     pango_font_description_from_string
;;;     pango_font_description_to_string
;;;     pango_font_description_to_filename

(test pango-font-description-from-string
  (let ((desc (pango:font-description-from-string "Sans Bold 16")))
    (is (typep desc 'pango:font-description))
    (is (string= "Sans Bold 16" (pango:font-description-to-string desc)))
    (is (string= "sans_bold_16" (pango:font-description-to-filename desc)))))

;;; 2024-9-18
