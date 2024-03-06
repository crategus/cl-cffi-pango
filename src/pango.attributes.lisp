;;; ----------------------------------------------------------------------------
;;; pango.attributes.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.51 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Text Attributes
;;;
;;;     Font and other attributes for annotating text
;;;
;;; Types and Values
;;;
;;;     PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING
;;;     PANGO_ATTR_INDEX_TO_TEXT_END
;;;
;;;     PANGO_SCALE_XX_SMALL
;;;     PANGO_SCALE_X_SMALL
;;;     PANGO_SCALE_SMALL
;;;     PANGO_SCALE_MEDIUM
;;;     PANGO_SCALE_LARGE
;;;     PANGO_SCALE_X_LARGE
;;;     PANGO_SCALE_XX_LARGE
;;;
;;;     PangoAttrType
;;;     PangoUnderline
;;;     PangoOverline                                      Since 1.46
;;;     PangoShowFlags                                     Since 1.44
;;;     PangoTextTransform                                 Since 1.50
;;;     PangoBaseLineShift                                 Since 1.50
;;;     PangoFontScale                                     Since 1.50
;;;
;;;     PangoAttrClass
;;;     PangoAttribute
;;;     PangoAttrString
;;;     PangoAttrLanguage
;;;     PangoAttrColor
;;;     PangoAttrInt
;;;     PangoAttrFloat
;;;     PangoAttrFontDesc
;;;     PangoAttrShape
;;;     PangoAttrSize
;;;     PangoAttrFontFeatures
;;;
;;;     PangoAttrList
;;;     PangoAttrIterator
;;;
;;; Functions
;;;
;;;     pango_attr_type_register
;;;     pango_attr_type_get_name
;;;
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
;;;     PangoAttrFilterFunc
;;;
;;;     pango_attr_list_new
;;;     pango_attr_list_ref
;;;     pango_attr_list_unref
;;;     pango_attr_list_copy
;;;     pango_attr_list_equal
;;;     pango_attr_list_insert
;;;     pango_attr_list_insert_before
;;;     pango_attr_list_change
;;;     pango_attr_list_splice
;;;     pango_attr_list_filter
;;;     pango_attr_list_update
;;;     pango_attr_list_get_attributes
;;;     pango_attr_list_from_string                        Since 1.50
;;;     pango_attr_list_to_string                          Since 1.50
;;;     pango_attr_list_get_iterator
;;;
;;;     pango_attr_iterator_copy
;;;     pango_attr_iterator_next
;;;     pango_attr_iterator_range
;;;     pango_attr_iterator_get
;;;     pango_attr_iterator_get_font
;;;     pango_attr_iterator_get_attrs
;;;     pango_attr_iterator_destroy
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ├── PangoAttrIterator
;;;     ├── PangoAttrList
;;;     ├── PangoAttribute
;;;     ╰── PangoColor
;;;
;;;     GEnum
;;;     ├── PangoAttrType
;;;     ├── PangoUnderline
;;;     ├── PangoOverline
;;;     ├── PangoTextTransform
;;;     ├── PangoBaseLineShift
;;;     ╰── PangoFontScale
;;;
;;;     GFlags
;;;     ╰── PangoShowFlags
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING
;;;
;;; #define PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING 0
;;;
;;; This value can be used to set the start_index member of a PangoAttribute
;;; such that the attribute covers from the beginning of the text.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ATTR_INDEX_TO_TEXT_END
;;;
;;; #define PANGO_ATTR_INDEX_TO_TEXT_END G_MAXUINT
;;;
;;; This value can be used to set the end_index member of a PangoAttribute such
;;; that the attribute covers to the end of the text.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_XX_SMALL
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale-xx-small+) "Constant")

(defconstant +scale-xx-small+ #.(/ 1.0d0 (* 1.2d0 1.2d0 1.2d0))
 #+liber-documentation
 "@version{2024-2-27}
  @variable-value{@code{(/ 1.0d0 (* 1.2d0 1.2d0 1.2d0))}}
  @begin{short}
    The scale factor for three shrinking steps.
  @end{short}")

(export '+scale-xx-small+)

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_X_SMALL
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale-x-small+) "Constant")

(defconstant +scale-x-small+ #.(/ 1.0d0 (* 1.2d0 1.2d0))
 #+liber-documentation
 "@version{2024-2-27}
  @variable-value{@code{(/ 1.0d0 (* 1.2d0 1.2d0))}}
  @begin{short}
    The scale factor for two shrinking steps.
  @end{short}")

(export '+scale-x-small+)

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_SMALL
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale-small+) "Constant")

(defconstant +scale-small+ #.(/ 1.0d0 1.2d0)
 #+liber-documentation
 "@version{2024-2-27}
  @variable-value{@code{(/ 1.0d0 1.2d0)}}
  @begin{short}
    The scale factor for one shrinking step.
  @end{short}")

(export '+scale-small+)

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_MEDIUM
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale-medium+) "Constant")

(defconstant +scale-medium+ 1.0d0
 #+liber-documentation
 "@version{2024-2-27}
  @variable-value{@code{1.0d0}}
  @begin{short}
    The scale factor for normal size.
  @end{short}")

(export '+scale-medium+)

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_LARGE
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale-large+) "Constant")

(defconstant +scale-large+ 1.2d0
 #+liber-documentation
 "@version{2024-2-27}
  @variable-value{@code{1.2d0}}
  @begin{short}
    The scale factor for one magnification step.
  @end{short}")

(export '+scale-large+)

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_X_LARGE
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale-x-large+) "Constant")

(defconstant +scale-x-large+ (* 1.2d0 1.2d0)
 #+liber-documentation
 "@version{2024-2-27}
  @variable-value{@code{(* 1.2d0 1.2d0)}}
  @begin{short}
    The scale factor for two magnification steps.
  @end{short}")

(export '+scale-x-large+)

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE_XX_LARGE
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale-xx-large+) "Constant")

(defconstant +scale-xx-large+ #.(* 1.2d0 1.2d0 1.2d0)
 #+liber-documentation
 "@version{2024-2-27}
  @variable-value{@code{(* 1.2d0 1.2d0 1.2d0)}}
  @begin{short}
    The scale factor for three magnification steps.
  @end{short}")

(export '+scale-xx-large+)

;;; ----------------------------------------------------------------------------
;;; enum PangoAttrType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoAttrType" attr-type
  (:export t
   :allow-undeclared-values t
   :type-initializer "pango_attr_type_get_type")
  (:invalid 0)
  (:language 1)
  (:family 2)
  (:style 3)
  (:weight 4)
  (:variant 5)
  (:stretch 6)
  (:size 7)
  (:font-desc 8)
  (:foreground 9)
  (:background 10)
  (:underline 11)
  (:strikethrough 12)
  (:rise 13)
  (:shape 14)
  (:scale 15)
  (:fallback 16)
  (:letter-spacing 17)
  (:underline-color 18)
  (:strikethrough-color 19)
  (:absolute-size 20)
  (:gravity 21)
  (:gravity-hint 22)
  (:font-features 23)
  (:foreground-alpha 24)
  (:background-alpha 25)
  #+pango-1-44
  (:allow-breaks 26)
  #+pango-1-44
  (:show 27)
  #+pango-1-44
  (:insert-hyphens 28)
  #+pango-1-46
  (:overline 29)
  #+pango-1-46
  (:overline-color 30)
  #+pango-1-50
  (:line-height 31)
  #+pango-1-50
  (:absolute-line-height 32)
  #+pango-1-50
  (:text-transform 33)
  #+pango-1-50
  (:word 34)
  #+pango-1-50
  (:sentence 35)
  #+pango-1-50
  (:baseline-shift 36)
  #+pango-1-50
  (:font-scale 37))

#+liber-documentation
(setf (liber:alias-for-symbol 'attr-type)
      "GEnum"
      (liber:symbol-documentation 'attr-type)
 "@version{2024-2-27}
  @begin{short}
    The @symbol{pango:attr-type} enumeration distinguishes between different
    types of attributes.
  @end{short}
  Along with the predefined values, it is possible to allocate additional
  values for custom attributes using the @fun{pango:attr-type-register}
  function. The predefined values are given below. The type of structure used
  to store the attribute is listed in parentheses after the description.
  @begin{pre}
(gobject:define-g-enum \"PangoAttrType\" attr-type
  (:export t
   :allow-undeclared-values t
   :type-initializer \"pango_attr_type_get_type\")
  (:invalid 0)
  (:language 1)
  (:family 2)
  (:style 3)
  (:weight 4)
  (:variant 5)
  (:stretch 6)
  (:size 7)
  (:font-desc 8)
  (:foreground 9)
  (:background 10)
  (:underline 11)
  (:strikethrough 12)
  (:rise 13)
  (:shape 14)
  (:scale 15)
  (:fallback 16)
  (:letter-spacing 17)
  (:underline-color 18)
  (:strikethrough-color 19)
  (:absolute-size 20)
  (:gravity 21)
  (:gravity-hint 22)
  (:font-features 23)
  (:foreground-alpha 24)
  (:background-alpha 25)
  (:allow-breaks 26)
  (:show 27)
  (:insert-hyphens 28)
  (:overline 29)
  (:overline-color 30)
  (:line-height 31)
  (:absolute-line-height 32)
  (:text-transform 33)
  (:word 34)
  (:sentence 35)
  (:baseline-shift 36)
  (:font-scale 37))
  @end{pre}
  @begin[code]{table}
    @entry[:invalid]{Does not happen.}
    @entry[:language]{Language.}
    @entry[:family]{Font family name list.}
    @entry[:style]{Font slant style.}
    @entry[:weight]{Font weight.}
    @entry[:variant]{Font variant, normal or small caps.}
    @entry[:stretch]{Font stretch.}
    @entry[:size]{Font size in points scaled by the @var{pango:+scale+} value.}
    @entry[:font-desc]{Font description.}
    @entry[:foreground]{Foreground color.}
    @entry[:background]{Background color.}
    @entry[:underline]{Whether the text has an underline.}
    @entry[:strikethrough]{Whether the text is struck-through.}
    @entry[:rise]{Baseline displacement.}
    @entry[:shape]{Shape.}
    @entry[:scale]{Font size scale factor.}
    @entry[:fallback]{Whether fallback is enabled.}
    @entry[:letter-spacing]{Letter spacing.}
    @entry[:underline-color]{Underline colo.}
    @entry[:strikethrough-color]{Strikethrough color.}
    @entry[:absolute-size]{Font size in pixels scaled by the
      @var{pango:+scale+} value.}
    @entry[:gravity]{Base text gravity.}
    @entry[:gravity-hint]{Gravity hint.}
    @entry[:font-features]{OpenType font features.}
    @entry[:foreground-alpha]{Foreground alpha.}
    @entry[:background-alpha]{Background alpha.}
    @entry[:allow-breaks]{Whether breaks are allowed. Since 1.44}
    @entry[:show]{How to render invisible characters. Since 1.44}
    @entry[:insert-hyphens]{Whether to insert hyphens at intra-word line breaks.
      Since 1.44}
    @entry[:overline]{Whether the text has an overline. Since 1.46}
    @entry[:overline-color]{Overline color. Since 1.46}
    @entry[:line-height]{Line height factor. Since 1.50}
    @entry[:absolute-line-height]{Line height. Since 1.50}
    @entry[:text-transform]{How Pango treats characters during shaping.
      Since 1.50}
    @entry[:word]{Override segmentation to classify the range of the attribute
      as a single word. Since 1.50}
    @entry[:sentence]{Override segmentation to classify the range of the
      attribute as a single sentence. Since 1.50}
    @entry[:baseline-shift]{Baseline displacement. Since 1.50}
    @entry[:font-scale]{Font-relative size change. Since 1.50}
  @end{table}
  @see-class{pango:attribute}
  @see-class{pango:attr-list}
  @see-function{pango:attr-type-register}")

;;; ----------------------------------------------------------------------------
;;; enum PangoUnderline
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoUnderline" underline
  (:export t
   :type-initializer "pango_underline_get_type")
  (:none 0)
  (:single 1)
  (:double 2)
  (:low 3)
  (:error 4)
  #+pango-1-46
  (:single-line 5)
  #+pango-1-46
  (:double-line 6)
  #+pango-1-46
  (:error-line 7))

#+liber-documentation
(setf (liber:alias-for-symbol 'underline)
      "GEnum"
      (liber:symbol-documentation 'underline)
 "@version{2023-1-19}
  @begin{short}
    The @symbol{pango:underline} enumeration is used to specify whether text
    should be underlined, and if so, the type of underlining.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"PangoUnderline\" underline
  (:export t
   :type-initializer \"pango_underline_get_type\")
  (:none 0)
  (:single 1)
  (:double 2)
  (:low 3)
  (:error 4)
  (:single-line 5)
  (:double-line 6)
  (:error-line 7))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No underline should be drawn.}
    @entry[:single]{A single underline should be drawn.}
    @entry[:double]{A double underline should be drawn.}
    @entry[:low]{A single underline should be drawn at a position beneath the
      ink extents of the text being underlined. This should be used only for
      underlining single characters, such as for keyboard accelerators. The
      @code{:single} value should be used for extended portions of text.}
    @entry[:error]{A wavy underline should be drawn below. This underline is
      typically used to indicate an error such as a possible mispelling. In
      some cases a contrasting color may automatically be used.}
    @entry[:single-line]{Like the @code{:single} value, but drawn continuously
      across multiple runs. Since 1.46}
    @entry[:double-line]{Like the @code{:double} value, but drawn continuously
      across multiple runs. Since 1.46}
    @entry[:error-line]{Like the @code{:error} value, but drawn continuously
      across multiple runs. Since 1.46}
  @end{table}
  @see-symbol{pango:overline}")

;;; ----------------------------------------------------------------------------
;;; enum PangoOverline
;;; ----------------------------------------------------------------------------

#+pango-1-46
(gobject:define-g-enum "PangoOverline" overline
  (:export t
   :type-initializer "pango_overline_get_type")
  (:none 0)
  (:single 1))

#+(and pango-1-46 liber-documentation)
(setf (liber:alias-for-symbol 'overline)
      "GEnum"
      (liber:symbol-documentation 'overline)
 "@version{2023-1-19}
  @begin{short}
    The @symbol{pango:overline} enumeration is used to specify whether text
    should be overlined, and if so, the type of line.
  @end{short}
  Since 1.46
  @begin{pre}
(gobject:define-g-enum \"PangoOverline\" overline
  (:export t
   :type-initializer \"pango_overline_get_type\")
  (:none 0)
  (:single 1))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No overline should be drawn.}
    @entry[:single]{Draw a single line above the ink extents of the text being
      underlined.}
  @end{table}
  @see-symbol{pango:underline}")

;;; ----------------------------------------------------------------------------
;;; enum PangoShowFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "PangoShowFlags" show-flags
  (:export t
   :type-initializer "pango_show_flags_get_type")
  (:none 0)
  (:spaces #.(ash 1 0))
  (:line-breaks #.(ash 1 1))
  (:ignorables #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'show-flags)
      "GFlags"
      (liber:symbol-documentation 'show-flags)
 "@version{2023-1-19}
  @begin{short}
    These flags affect how Pango treats characters that are normally not
    visible in the output.
  @end{short}
  @begin{pre}
(gobject:define-g-flags \"PangoShowFlags\" show-flags
  (:export t
   :type-initializer \"pango_show_flags_get_type\")
  (:none 0)
  (:spaces #.(ash 1 0))
  (:line-breaks #.(ash 1 1))
  (:ignorables #.(ash 1 2)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No special treatment for invisible characters.}
    @entry[:spaces]{Render spaces, tabs and newlines visibly.}
    @entry[:line-breaks]{Render line breaks visibly.}
    @entry[:ignorables]{Render default-ignorable Unicode characters visibly.}
  @end{table}
  @see-function{pango:attr-show-new}")

;;; ----------------------------------------------------------------------------
;;; enum PangoTextTransform
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoTextTransform" text-transform
  (:export t
   :type-initializer "pango_text_transform_get_type")
  :none
  :lowercase
  :uppercase
  :capitalize)

#+liber-documentation
(setf (liber:alias-for-symbol 'text-transform)
      "GEnum"
      (liber:symbol-documentation 'text-transform)
 "@version{2023-1-19}
  @begin{short}
    An enumeration that affects how Pango treats characters during shaping.
  @end{short}
  Since 1.50
  @begin{pre}
(gobject:define-g-enum \"PangoTextTransform\" text-transform
  (:export t
   :type-initializer \"pango_text_transform_get_type\")
  :none
  :lowercase
  :uppercase
  :capitalize)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Leave text unchanged.}
    @entry[:lowercase]{Display letters and numbers as lowercase.}
    @entry[:uppercase]{Display letters and numbers as uppercase.}
    @entry[:capitalize]{Display the first character of a word in titlecase.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum PangoBaselineShift
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoBaselineShift" baseline-shift
  (:export t
   :type-initializer "pango_baseline_shift_get_type")
  :none
  :superscript
  :subscript)

#+liber-documentation
(setf (liber:alias-for-symbol 'baseline-shift)
      "GEnum"
      (liber:symbol-documentation 'baseline-shift)
 "@version{2023-1-19}
  @begin{short}
    An enumeration that affects baseline shifts between runs.
  @end{short}
  Since 1.50
  @begin{pre}
(gobject:define-g-enum \"PangoBaselineShift\" baseline-shift
  (:export t
   :type-initializer \"pango_baseline_shift_get_type\")
  :none
  :superscript
  :subscript)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Leave the baseline unchanged.}
    @entry[:superscript]{Shift the baseline to the superscript position,
      relative to the previous run.}
    @entry[:subscript]{Shift the baseline to the subscript position, relative
      to the previous run.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; enum PangoFontScale
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoFontScale" font-scale
  (:export t
   :type-initializer "pango_font_scale_get_type")
  :none
  :superscript
  :subscript
  :small-caps)

#+liber-documentation
(setf (liber:alias-for-symbol 'font-scale)
      "GEnum"
      (liber:symbol-documentation 'font-scale)
 "@version{2023-1-19}
  @begin{short}
    An enumeration that affects font sizes for superscript and subscript
    positioning and for (emulated) Small Caps.
  @end{short}
  Since 1.50
  @begin{pre}
(gobject:define-g-enum \"PangoFontScale\" font-scale
  (:export t
   :type-initializer \"pango_font_scale_get_type\")
  :none
  :superscript
  :subscript
  :small-caps)
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Leave the font size unchanged.}
    @entry[:superscript]{Change the font to a size suitable for superscripts.}
    @entry[:subscript]{Change the font to a size suitable for subscripts.}
    @entry[:small-caps]{Change the font to a size suitable for Small Caps.}
  @end{table}")

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrClass                                  not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %attr-class
  (type attr-type)
  (copy :pointer)
  (destroy :pointer)
  (equal :pointer))

;;; ----------------------------------------------------------------------------
;;; struct PangoAttribute
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %attribute
  (klass :pointer)
  (start-index :uint)
  (end-index :uint))

(glib:define-g-boxed-opaque attribute "PangoAttribute"
   :export t
   :type-initializer "pango_attribute_get_type"
   :alloc (error "PangoAttribute cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'attribute)
      "GBoxed"
      (documentation 'attribute 'type)
 "@version{#2023-2-5}
  @begin{short}
    The @class{pango:attribute} structure represents the common portions of all
    attributes.
  @end{short}
  Particular types of attributes include this structure as their initial
  portion. The common portion of the attribute holds the range to which the
  value in the type specific part of the attribute applies. By default an
  attribute will have an all inclusive range of @code{[0,G_MAXUINT]}.
  @see-slot{pango:attribute-start-index}
  @see-slot{pango:attribute-end-index}
  @see-slot{pango:attribute-type}
  @see-symbol{pango:attr-type}
  @see-function{pango:attribute-init}")

;;; --- pango:attribute-type ---------------------------------------------------

;; Get the type of the PangoAttribute from the KLASS slot

(defun attribute-type (attr)
  (let* ((ptr (glib:boxed-opaque-pointer attr))
         (klass (cffi:foreign-slot-value ptr
                                         '(:struct %attribute)
                                         'klass)))
    (cffi:foreign-slot-value klass '(:struct %attr-class) 'type)))

#+liber-documentation
(setf (liber:alias-for-function 'attribute-type)
      "Accessor"
      (documentation 'attribute-type 'function)
 "@version{2024-2-27}
  @syntax[]{(pango:attribute-type instance) => type}
  @argument[instance]{a @class{pango:attribute} instance}
  @argument[type]{a @symbol{pango:attr-type} value}
  @begin{short}
    Accessor of the @code{type} information of the @class{pango:attribute}
    structure.
  @end{short}
  @see-class{pango:attribute}")

(export 'attribute-type)

;;; --- pango:attribute-start-index --------------------------------------------

(defun (setf attribute-start-index) (value attribute)
  (let ((ptr (glib:boxed-opaque-pointer attribute)))
    (setf (cffi:foreign-slot-value ptr '(:struct %attribute) 'start-index)
          value)))

(defun attribute-start-index (attribute)
  (let ((ptr (glib:boxed-opaque-pointer attribute)))
    (cffi:foreign-slot-value ptr '(:struct %attribute) 'start-index)))

#+liber-documentation
(setf (liber:alias-for-function 'attribute-start-index)
      "Accessor"
      (documentation 'attribute-start-index 'function)
 "@version{2024-2-27}
  @syntax[]{(pango:attribute-start-index instance) => index}
  @syntax[]{(setf (pango:attribute-start-index instance) index)}
  @argument[instance]{a @class{pango:attribute} instance}
  @argument[index]{an integer with the start index}
  @begin{short}
    Accessor of the @code{start-index} slot of the @class{pango:attribute}
    structure.
  @end{short}
  @see-class{pango:attribute}")

(export 'attribute-start-index)

;;; --- pango:attribute-end-index ----------------------------------------------

(defun (setf attribute-end-index) (value attribute)
  (let ((ptr (glib:boxed-opaque-pointer attribute)))
    (setf (cffi:foreign-slot-value ptr '(:struct %attribute) 'end-index)
          value)))

(defun attribute-end-index (attribute)
  (let ((ptr (glib:boxed-opaque-pointer attribute)))
    (cffi:foreign-slot-value ptr '(:struct %attribute) 'end-index)))

#+liber-documentation
(setf (liber:alias-for-function 'attribute-end-index)
      "Accessor"
      (documentation 'attribute-end-index 'function)
 "@version{2024-2-27}
  @syntax[]{(pango:attribute-end-index instance) => index}
  @syntax[]{(setf (pango:attribute-end-index instance) index)}
  @argument[instance]{a @class{pango:attribute} instance}
  @argument[index]{an integer with the start index}
  @begin{short}
    Accessor of the @code{end-index} slot of the @class{pango:attribute}
    structure.
  @end{short}
  @see-class{pango:attribute}")

(export 'attribute-end-index)

;;; ----------------------------------------------------------------------------
;;; pango_attribute_init ()
;;;
;;; void pango_attribute_init (PangoAttribute *attr,
;;;                            const PangoAttrClass *klass);
;;;
;;; Initializes attr's klass to klass, it's start_index to
;;; PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING and end_index to
;;; PANGO_ATTR_INDEX_TO_TEXT_END such that the attribute applies to the entire
;;; text by default.
;;;
;;; attr :
;;;     a PangoAttribute
;;;
;;; klass :
;;;     a PangoAttributeClass
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attribute_copy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attribute_copy" attribute-copy)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2023-7-17}
  @argument[attr]{a @class{pango:attribute} instance}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Makes a copy of an attribute.}
  @see-classs{pango:attribute}"
  (attr (g:boxed attribute)))

(export 'attribute-copy)

;;; ----------------------------------------------------------------------------
;;; pango_attribute_equal ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attribute_equal" attribute-equal) :boolean
 #+liber-documentation
 "@version{2023-7-17}
  @argument[attr1]{a @class{pango:attribute} instance}
  @argument[attr2]{a @class{pango:attribute} instance}
  @return{@em{True} if the two attributes have the same value.}
  @begin{short}
    Compare two attributes for equality.
  @end{short}
  This compares only the actual value of the two attributes and not the ranges
  that the attributes apply to.
  @see-class{pango:attribute}"
  (attr1 (g:boxed attribute))
  (attr2 (g:boxed attribute)))

(export 'attribute-equal)

;;; ----------------------------------------------------------------------------
;;; pango_attribute_destroy ()
;;;
;;; void pango_attribute_destroy (PangoAttribute *attr);
;;;
;;; Destroy a PangoAttribute and free all associated memory.
;;;
;;; attr :
;;;     a PangoAttribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrString
;;;
;;; struct PangoAttrString {
;;;   PangoAttribute attr;
;;;   char *value;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; char *value;
;;;     the string which is the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrLanguage
;;;
;;; struct PangoAttrLanguage {
;;;   PangoAttribute attr;
;;;   PangoLanguage *value;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; PangoLanguage *value;
;;;     the PangoLanguage which is the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrColor
;;;
;;; struct PangoAttrColor {
;;;   PangoAttribute attr;
;;;   PangoColor color;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; PangoColor color;
;;;     the PangoColor which is the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrInt
;;;
;;; struct PangoAttrInt {
;;;   PangoAttribute attr;
;;;   int value;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; int value;
;;;     the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrFloat
;;;
;;; struct PangoAttrFloat {
;;;   PangoAttribute attr;
;;;   double value;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; double value;
;;;     the value of the attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrFontDesc
;;;
;;; struct PangoAttrFontDesc {
;;;   PangoAttribute attr;
;;;   PangoFontDescription *desc;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; PangoFontDescription *desc;
;;;     the font description which is the value of this attribute
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrShape
;;;
;;; struct PangoAttrShape {
;;;   PangoAttribute attr;
;;;   PangoRectangle ink_rect;
;;;   PangoRectangle logical_rect;
;;;
;;;   gpointer              data;
;;;   PangoAttrDataCopyFunc copy_func;
;;;   GDestroyNotify        destroy_func;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; PangoRectangle ink_rect;
;;;     the ink rectangle to restrict to
;;;
;;; PangoRectangle logical_rect;
;;;     the logical rectangle to restrict to
;;;
;;; gpointer data;
;;;     user data set (see pango_attr_shape_new_with_data())
;;;
;;; PangoAttrDataCopyFunc copy_func;
;;;     copy function for the user data
;;;
;;; GDestroyNotify destroy_func;
;;;     destroy function for the user data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrSize
;;;
;;; struct PangoAttrSize {
;;;   PangoAttribute attr;
;;;   int size;
;;;   guint absolute : 1;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; int size;
;;;     size of font, in units of 1/PANGO_SCALE of a point (for PANGO_ATTR_SIZE)
;;;     or of a device uni (for PANGO_ATTR_ABSOLUTE_SIZE)
;;;
;;; guint absolute : 1;
;;;     whether the font size is in device units or points. This field is only
;;;     present for compatibility with Pango-1.8.0 (PANGO_ATTR_ABSOLUTE_SIZE
;;;     was added in 1.8.1); and always will be FALSE for PANGO_ATTR_SIZE and
;;;     TRUE for PANGO_ATTR_ABSOLUTE_SIZE.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoAttrFontFeatures
;;;
;;; struct PangoAttrFontFeatures {
;;;   PangoAttribute attr;
;;;   gchar *features;
;;; };
;;;
;;; PangoAttribute attr;
;;;     the common portion of the attribute
;;;
;;; gchar *features;
;;;     the featues, as a string in CSS syntax
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_type_register ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_type_register" attr-type-register) attr-type
 #+liber-documentation
 "@version{2024-2-27}
  @argument[name]{a string with an identifier for the type}
  @return{The integer with the new type ID.}
  @begin{short}
    Allocates a new attribute type ID.
  @end{short}
  The attribute type name can be accessed later by using the
  @fun{pango:attr-type-name} function.
  @see-symbol{pango:attr-type}
  @see-function{pango:attr-type-name}"
  (name (:string :free-to-foreign nil)))

(export 'attr-type-register)

;;; ----------------------------------------------------------------------------
;;; pango_attr_type_get_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_type_get_name" attr-type-name)
    (:string :free-from-foreign nil)
  #+liber-documentation
  "@version{2024-2-27}
   @argument[type]{an integer with an attribute type ID to fetch the name for}
   @return{The string with the type ID name, or @code{nil} if @arg{type} is a
     built-in Pango attribute type or invalid.}
  @begin{short}
    Fetches the attribute type name passed in when registering the type using
    the @fun{pango:attr-type-register} function.
  @end{short}
  The returned value is an interned string that should not be modified.
  @see-symbol{pango:attr-type}
  @see-function{pango:attr-type-register}"
  (type attr-type))

(export 'attr-type-name)

;;; ----------------------------------------------------------------------------
;;; pango_attr_language_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_language_new" attr-language-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-26}
  @argument[language]{a @class{pango:language} instance}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new language tag attribute.}
  @see-class{pango:attribute}
  @see-class{pango:language}"
  (language (g:boxed language)))

(export 'attr-language-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_family_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_family_new" attr-family-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-26}
  @argument[family]{a string with the family or comma separated list of
    families}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font family attribute.}
  @see-class{pango:attribute}"
  (family :string))

(export 'attr-family-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_style_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_style_new" attr-style-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-26}
  @argument[style]{a @symbol{pango:style} value with the slant style}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font slant style attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:style}"
  (style style))

(export 'attr-style-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_variant_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_variant_new" attr-variant-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-26}
  @argument[variant]{a @symbol{pango:variant} value with the variant}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font variant attribute (normal or small caps).}
  @see-class{pango:attribute}
  @see-symbol{pango:variant}"
  (variant variant))

(export 'attr-variant-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_stretch_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_stretch_new" attr-stretch-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-26}
  @argument[stretch]{a @symbol{pango:stretch} value with the stretch}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font stretch attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:stretch}"
  (stretch stretch))

(export 'attr-stretch-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_weight_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_weight_new" attr-weight-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[weight]{a @symbol{pango:weight} value with the weight}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font weight attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:weight}"
  (weight weight))

(export 'attr-weight-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_size_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_size_new" attr-size-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[size]{an integer with the font size, in @var{pango:+scale+} units
    of a point}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font-size attribute in fractional points.}
  @see-class{pango:attribute}"
  (size :int))

(export 'attr-size-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_size_new_absolute ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_size_new_absolute" attr-size-new-absolute)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[size]{an integer with the font size, in @var{pango:+scale+} units
    of device units}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font-size attribute in device units.}
  @see-class{pango:attribute}"
  (size :int))

(export 'attr-size-new-absolute)

;;; ----------------------------------------------------------------------------
;;; pango_attr_font_desc_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_font_desc_new" attr-font-desc-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[desc]{a @class{pango:font-description} instance}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new font description attribute.
  @end{short}
  This attribute allows setting family, style, weight, variant, stretch, and
  size simultaneously.
  @see-class{pango:attribute}
  @see-class{pango:font-description}"
  (desc (g:boxed font-description)))

(export 'attr-font-desc-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_foreground_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_foreground_new" attr-foreground-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[red]{an integer with the red value, ranging from 0 to 65535}
  @argument[green]{an integer with the green value}
  @argument[blue]{an integer with the blue value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new foreground color attribute.
  @end{short}
  @see-class{pango:attribute}"
  (red :uint16)
  (green :uint16)
  (blue :uint16))

(export 'attr-foreground-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_background_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_background_new" attr-background-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[red]{an integer with the red value, ranging from 0 to 65535}
  @argument[green]{an integer with the green value}
  @argument[blue]{an integer with the blue value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new background color attribute.
  @end{short}
  @see-class{pango:attribute}"
  (red :uint16)
  (green :uint16)
  (blue :uint16))

(export 'attr-background-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_strikethrough_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_strikethrough_new" attr-strikethrough-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[strikethrough]{@em{true} if the text should be struck-through}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new strikethrough attribute.
  @end{short}
  @see-class{pango:attribute}"
  (strikethrough :boolean))

(export 'attr-strikethrough-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_strikethrough_color_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_strikethrough_color_new"
                attr-strikethrough-color-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[red]{an integer with the red value, ranging from 0 to 65535}
  @argument[green]{an integer with the green value}
  @argument[blue]{an integer with the blue value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new strikethrough color attribute.
  @end{short}
  This attribute modifies the color of strikethrough lines. If not set,
  strikethrough lines will use the foreground color.
  @see-class{pango:attribute}"
  (red :uint16)
  (green :uint16)
  (blue :uint16))

(export 'attr-strikethrough-color-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_underline_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_underline_new" attr-underline-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[underline]{a @symbol{pango:underline} value with the underline
    style}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new underline style attribute.
  @end{short}
  @see-class{pango:attribute}
  @see-symbol{pango:underline}"
  (underline underline))

(export 'attr-underline-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_underline_color_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_underline_color_new" attr-underline-color-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[red]{an integer with the red value, ranging from 0 to 65535}
  @argument[green]{an integer with the green value}
  @argument[blue]{an integer with the blue value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new underline color attribute.
  @end{short}
  This attribute modifies the color of underlines. If not set, underlines will
  use the foreground color.
  @see-class{pango:attribute}"
  (red :uint16)
  (green :uint16)
  (blue :uint16))

(export 'attr-underline-color-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_overline_new ()
;;; ----------------------------------------------------------------------------

#+pango-1-46
(cffi:defcfun ("pango_attr_overline_new" attr-overline-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[overline]{a @symbol{pango:overline} value with the overline style}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new overline style attribute.
  @end{short}

  Since 1.46
  @see-class{pango:attribute}
  @see-symbol{pango:overline}"
  (overline overline))

#+pango-1-46
(export 'attr-overline-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_overline_color_new ()
;;; ----------------------------------------------------------------------------

#+pango-1-46
(cffi:defcfun ("pango_attr_overline_color_new" attr-overline-color-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[red]{an integer with the red value, ranging from 0 to 65535}
  @argument[green]{an integer with the green value}
  @argument[blue]{an integer with the blue value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new overline color attribute.
  @end{short}
  This attribute modifies the color of overlines. If not set, overlines will
  use the foreground color.

  Since 1.46
  @see-class{pango:attribute}"
  (red :uint16)
  (green :uint16)
  (blue :uint16))

#+pango-1-46
(export 'attr-overline-color-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_shape_new ()
;;; ----------------------------------------------------------------------------

;; TODO: Improve the implementation of pango:rectangle

(cffi:defcfun ("pango_attr_shape_new" attr-shape-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[ink-rect]{a @symbol{pango:rectangle} instance to assign to each
    character}
  @argument[logical-rect]{a @symbol{pango:rectangle} instance to assign to
    each character}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Creates a new shape attribute.
  @end{short}
  A shape is used to impose a particular ink and logical rectangle on the result
  of shaping a particular glyph. This might be used, for instance, for embedding
  a picture or a widget inside a Pango layout.
  @see-class{pango:attribute}
  @see-symbol{pango:rectangle}"
  (ink-rect (:pointer (:struct rectangle)))
  (logical-rect (:pointer (:struct rectangle))))

(export 'attr-shape-new)

;;; ----------------------------------------------------------------------------
;;; PangoAttrDataCopyFunc ()
;;;
;;; gpointer (*PangoAttrDataCopyFunc) (gconstpointer data);
;;;
;;; A copy function passed to attribute new functions that take user data.
;;;
;;; data :
;;;     the user data
;;;
;;; Returns :
;;;     a new copy of data
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_shape_new_with_data ()
;;;
;;; PangoAttribute * pango_attr_shape_new_with_data
;;;                                         (const PangoRectangle *ink_rect,
;;;                                          const PangoRectangle *logical_rect,
;;;                                          gpointer data,
;;;                                          PangoAttrDataCopyFunc copy_func,
;;;                                          GDestroyNotify destroy_func);
;;;
;;; Like pango_attr_shape_new(), but a user data pointer is also provided; this
;;; pointer can be accessed when later rendering the glyph.
;;;
;;; ink_rect :
;;;     ink rectangle to assign to each character
;;;
;;; logical_rect :
;;;     logical rectangle to assign to each character
;;;
;;; data :
;;;     user data pointer
;;;
;;; copy_func :
;;;     function to copy data when the attribute is copied. If NULL, data is
;;;     simply copied as a pointer
;;;
;;; destroy_func :
;;;     function to free data when the attribute is freed, or NULL
;;;
;;; Returns :
;;;     the newly allocated PangoAttribute, which should be freed with
;;;     pango_attribute_destroy().
;;;
;;; Since 1.8
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_scale_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_scale_new" %attr-scale-new)
    (g:boxed attribute :return)
  (factor :double))

(defun attr-scale-new (factor)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[factor]{a number coerced to a double float with the factor to scale
    the font}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new font size scale attribute.
  @end{short}
  The base font for the affected text will have its size multiplied by
  @arg{factor}.
  @see-class{pango:attribute}"
  (%attr-scale-new (coerce factor 'double-float)))

(export 'attr-scale-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_rise_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_rise_new" attr-rise-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[rise]{an integer with the amount that the text should be displaced
    vertically, in Pango units, positive values displace the text upwards}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new baseline displacement attribute.
  @end{short}
  @see-class{pango:attribute}"
  (rise :int))

(export 'attr-rise-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_letter_spacing_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_letter_spacing_new" attr-letter-spacing-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[spacing]{an integer with the amount of extra space to add between
    graphemes of the text, in Pango units}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new letter spacing attribute.
  @end{short}
  @see-class{pango:attribute}"
  (spacing :int))

(export 'attr-letter-spacing-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_fallback_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_fallback_new" attr-fallback-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[fallback]{@em{true} if we should fall back on other fonts for
    characters the active font is missing}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new font fallback attribute.
  @end{short}
  If @arg{fallback} is disabled, characters will only be used from the closest
  matching font on the system. No fallback will be done to other fonts on the
  system that might contain the characters in the text.
  @see-class{pango:attribute}"
  (enable :boolean))

(export 'attr-fallback-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_gravity_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_gravity_new" attr-gravity-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[gravity]{a @symbol{pango:gravity} value, should not be @code{:auto}}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new gravity attribute.
  @end{short}
  @see-class{pango:attribute}
  @see-symbol{pango:gravity}"
  (gravity gravity))

(export 'attr-gravity-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_gravity_hint_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_gravity_hint_new" attr-gravity-hint-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[hint]{a @symbol{pango:gravity-hint} value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new gravity hint attribute.
  @end{short}
  @see-class{pango:attribute}
  @see-symbol{pango:gravity-hint}"
  (hint gravity-hint))

(export 'attr-gravity-hint-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_font_features_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_font_features_new" attr-font-features-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[features]{a string with OpenType font features, in CSS syntax}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new font features tag attribute.
  @end{short}
  @see-class{pango:attribute}"
  (features :string))

(export 'attr-font-features-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_foreground_alpha_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_foreground_alpha_new" attr-foreground-alpha-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[alpha]{an unsigned integer with the alpha value, between 1 and
    65536}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new foreground alpha attribute.
  @end{short}
  @see-class{pango:attribute}"
  (alpha :uint16))

(export 'attr-foreground-alpha-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_background_alpha_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_background_alpha_new" attr-background-alpha-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[alpha]{an unsigned integer with the alpha value, between 1 and
    65536}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new background alpha attribute.
  @end{short}
  @see-class{pango:attribute}"
  (alpha :uint16))

(export 'attr-background-alpha-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_allow_breaks_new ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(cffi:defcfun ("pango_attr_allow_breaks_new" attr-allow-breaks-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[allow]{@em{true} if line breaks are allowed}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new allow breaks attribute.
  @end{short}
  If breaks are disabled, the range will be kept in a single run, as far as
  possible.

  Since 1.44
  @see-class{pango:attribute}"
  (allow :boolean))

#+pango-1-44
(export 'attr-allow-breaks-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_insert_hyphens_new ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(cffi:defcfun ("pango_attr_insert_hyphens_new" attr-insert-hyphens-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[hyphens]{@em{true} if hyphens should be inserted}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new insert hyphens attribute.
  @end{short}
  Pango will insert hyphens when breaking lines in the middle of a word. This
  attribute can be used to suppress the hyphen.

  Since 1.44
  @see-class{pango:attribute}"
  (insert :boolean))

#+pango-1-44
(export 'attr-insert-hyphens-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_show_new ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(cffi:defcfun ("pango_attr_show_new" attr-show-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2023-7-17}
  @argument[flags]{a @symbol{pango:show-flags} value to apply.}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new attribute that influences how invisible characters are
    rendered.
  @end{short}

  Since 1.44
  @see-class{pango:attribute}
  @see-symbol{pango:show-flags}"
  (flags show-flags))

#+pango-1-44
(export 'attr-show-new)

;;; ----------------------------------------------------------------------------
;;; PangoAttrIterator
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque attr-iterator "PangoAttrIterator"
  :export t
  :type-initializer "pango_attr_iterator_get_type"
  :alloc (error "PangoAttrIterator cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'attr-iterator)
      "GBoxed"
      (documentation 'attr-iterator 'type)
 "@version{2024-2-27}
  @begin{short}
    The @class{pango:attr-iterator} structure is used to represent an iterator
    through a @class{pango:attr-list} instance.
  @end{short}
  The @class{pango:attr-iterator} structure is opaque, and has no user visible
  fields. A new iterator is created with the @fun{pango:attr-list-iterator}
  function. Once the iterator is created, it can be advanced through the style
  changes in the text using the @fun{pango:attr-iterator-next} function. At
  each style change, the range of the current style segment and the attributes
  currently in effect can be queried.
  @see-constructor{pango:attr-list-iterator}
  @see-constructor{pango:attr-iterator-copy}
  @see-class{pango:attr-list}
  @see-function{pango:attr-iterator-next}")

;;; ----------------------------------------------------------------------------
;;; PangoAttrList
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque attr-list "PangoAttrList"
  :type-initializer "pango_attr_list_get_type"
  :alloc (%attr-list-new))

#+liber-documentation
(setf (liber:alias-for-class 'attr-list)
      "GBoxed"
      (documentation 'attr-list 'type)
 "@version{2024-2-27}
  @begin{short}
    The @class{pango:attr-list} structure represents a list of attributes that
    apply to a section of text.
  @end{short}
  The @class{pango:attr-list} structure is opaque, and has no user visible
  fields. The attributes are, in general, allowed to overlap in an arbitrary
  fashion, however, if the attributes are manipulated only through the
  @fun{pango:attr-list-change} function, the overlap between properties will
  meet stricter criteria.

  Since the @class{pango:attr-list} structure is stored as a linear list, it is
  not suitable for storing attributes for large amounts of text. In general,
  you should not use a single @class{pango:attr-list} instance for more than
  one paragraph of text.
  @see-class{pango:attr-iterator}
  @see-function{pango:attr-list-change}")

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_new" %attr-list-new) :pointer)

(cffi:defcfun ("pango_attr_list_new" attr-list-new)
    (g:boxed attr-list :return)
 #+liber-documentation
 "@version{2024-2-27}
  @return{The newly allocated @class{pango:attr-list} instance.}
  @begin{short}
    Create a new empty attribute list.
  @end{short}
  @see-class{pango:attr-list}")

(export 'attr-list-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_ref ()
;;;
;;; PangoAttrList * pango_attr_list_ref (PangoAttrList *list);
;;;
;;; Increase the reference count of the given attribute list by one.
;;;
;;; list :
;;;     a PangoAttrList, may be NULL
;;;
;;; Returns :
;;;     The attribute list passed in
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_unref ()
;;;
;;; void pango_attr_list_unref (PangoAttrList *list);
;;;
;;; Decrease the reference count of the given attribute list by one. If the
;;; result is zero, free the attribute list and the attributes it contains.
;;;
;;; list :
;;;     a PangoAttrList, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_copy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_copy" attr-list-copy)
    (g:boxed attr-list :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[attrlist]{a @class{pango:attr-list} instance, may be @code{nil}}
  @begin{return}
    The newly allocated @class{pango:attr-list} instance. Returns @code{nil}
    if @arg{attr-list} was @code{nil}.
  @end{return}
  @begin{short}
    Copy @arg{attrlist} and return an identical new attribute list.
  @end{short}
  @see-class{pango:attr-list}"
  (attrlist (g:boxed attr-list)))

(export 'attr-list-copy)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_equal ()
;;; ----------------------------------------------------------------------------

#+pango-1-46
(cffi:defcfun ("pango_attr_list_equal" attr-list-equal) :boolean
 #+liber-documentation
 "@version{2024-2-27}
  @argument[attrlist1]{a @class{pango:attr-list} instance}
  @argument[attrlist2]{a @class{pango:attr-list} instance}
  @begin{short}
    Checks whether @arg{listattr1} and @arg{listattr2} contain the same
    attributes and whether those attributes apply to the same ranges.
  @end{short}
  Beware that this will return wrong values if any list contains duplicates.

  Since 1.46
  @see-class{pango:attr-list}"
  (attrlist1 (g:boxed attr-list))
  (attrlist2 (g:boxed attr-list)))

#+pango-1-46
(export 'attr-list-equal)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_insert ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_insert" attr-list-insert) :void
 #+liber-documentation
 "@version{2024-2-27}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[attr]{a @class{pango:attribute} instance}
  @begin{short}
    Insert the given attribute into the @class{pango:attr-list} instance.
  @end{short}
  It will be inserted after all other attributes with a matching
  @code{start-index}.
  @see-class{pango:attr-list}
  @see-class{pango:attribute}"
  (attrlist (g:boxed attr-list))
  (attr (g:boxed attribute)))

(export 'attr-list-insert)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_insert_before ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_insert_before" attr-list-insert-before) :void
 #+liber-documentation
 "@version{2024-2-28}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[attr]{a @class{pango:attribute} instance to insert}
  @begin{short}
    Insert the given attribute into the @class{pango:attr-list} instance.
  @end{short}
  It will be inserted before all other attributes with a matching start index.
  @see-class{pango:attr-list}
  @see-class{pango:attribute}"
  (attrlist (g:boxed attr-list))
  (attr (g:boxed attribute)))

(export 'attr-list-insert-before)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_change ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_change" attr-list-change) :void
 #+liber-documentation
 "@version{2024-2-28}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[attr]{a @class{pango:attribute} instance}
  @begin{short}
    Insert the given attribute into the @class{pango:attr-list} instance.
  @end{short}
  It will replace any attributes of the same type on that segment and be merged
  with any adjoining attributes that are identical.

  This function is slower than the @fun{pango:attr-list-insert} function for
  creating a attribute list in order, potentially much slower for large lists.
  However, the @fun{pango:attr-list-insert} function is not suitable for
  continually changing a set of attributes since it never removes or combines
  existing attributes.
  @see-class{pango:attr-list}
  @see-class{pango:attribute}
  @see-function{pango:attr-list-insert}"
  (attrlist (g:boxed attr-list))
  (attr (g:boxed attribute)))

(export 'attr-list-change)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_splice ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_splice" attr-list-splice) :void
 #+liber-documentation
 "@version{#2024-2-28}
  @argument[attrlist1]{a @class{pango:attr-list} instance}
  @argument[attrlist2]{a @class{pango:attr-list} instance}
  @argument[pos]{an integer with the position in the attribute list at which
    to insert @arg{attrlist2}}
  @argument[len]{an integer with the length of the spliced segment, note that
    this must be specified since the attributes in @arg{attrlist2} may only be
    present at some subsection of this range}
  @begin{short}
    This function opens up a hole in the attribute list, fills it in with
    attributes from the left, and then merges @arg{attrlist2} on top of the
    hole.
  @end{short}
  This operation is equivalent to stretching every attribute that applies at
  position @arg{pos} in the attribute list by an amount @arg{len}, and then
  calling the @fun{pango:attr-list-change} function with a copy of each
  attribute in other in sequence, offset in position by @arg{pos}.

  This operation proves useful for, for instance, inserting a pre-edit string
  in the middle of an edit buffer.
  @see-class{pango:attr-list}
  @see-function{pango:attr-list-change}"
  (attrlist1 (g:boxed attr-list))
  (attrlist2 (g:boxed attr-list))
  (pos :int)
  (len :int))

(export 'attr-list-splice)

;;; ----------------------------------------------------------------------------
;;; PangoAttrFilterFunc ()
;;; ----------------------------------------------------------------------------

(cffi:defcallback attr-filter-func :boolean
    ((attribute (g:boxed attribute))
     (data :pointer))
  (restart-case
    (let ((ptr (glib:get-stable-pointer-value data)))
      (funcall ptr attribute))
    (return-from-callback () nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'attr-filter-func)
      "Callback"
      (liber:symbol-documentation 'attr-filter-func)
 "@version{#2024-2-28}
  @argument[attribute]{a @class{pango:attribute} instance}
  @return{@em{True} if the attribute should be filtered out.}
  @begin{short}
    A predicate callback function used by the @fun{pango:attr-list-filter}
    function to filter out a subset of attributes for an attribute list.
  @end{short}
  @see-class{pango:attribute}
  @see-function{pango:attr-list-filter}")

(export 'attr-filter-func)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_filter ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_filter" %attr-list-filter)
    (g:boxed attr-list :return)
  (attrlist (g:boxed attr-list))
  (func :pointer)
  (data :pointer))

(defun attr-list-filter (attrlist func)
 #+liber-documentation
 "@version{#2024-2-28}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[func]{a @symbol{pango:filter-func} callback function, returns
    @em{true} if an attribute should be filtered out}
  @return{The new @class{pango:attr-list} instance or @code{nil} if no
    attributes of the given types were found.}
  @begin{short}
    Given a attribute list and callback function, removes any elements of the
    attribute list for which @arg{func} returns @em{true} and inserts them into
    a new attribute list.
  @end{short}
  @see-class{pango:attr-list}
  @see-symbol{pango:attr-filter-func}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (unwind-protect
      (%attr-list-filter attrlist
                         (cffi:callback attr-filter-func)
                         ptr)
      (glib:free-stable-pointer ptr))))

(export 'attr-list-filter)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_update ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(cffi:defcfun ("pango_attr_list_update" attr-list-update) :void
 #+liber-documentation
 "@version{#2024-2-28}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[pos]{an integer with the position of the change}
  @argument[remove]{an integer with the number of removed bytes}
  @argument[add]{an integer with the number of added bytes}
  @begin{short}
    Update indices of attributes in the attribute list for a change in the text
    they refer to.
  @end{short}
  The change that this function applies is removing @arg{remove} bytes at
  position @arg{pos} and inserting @arg{add} bytes instead.

  Attributes that fall entirely in the @code{(pos, pos + remove)} range are
  removed. Attributes that start or end inside the @code{(pos, pos + remove)}
  range are shortened to reflect the removal. Attributes @code{start} and
  @code{end} positions are updated if they are behind @code{pos + remove}.

  Since 1.44
  @see-class{pango:attr-list}"
  (attrlist (g:boxed attr-list))
  (pos :int)
  (remove :int)
  (add :int))

#+pango-1-44
(export 'attr-list-update)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_get_attributes ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(cffi:defcfun ("pango_attr_list_get_attributes" attr-list-attributes)
    (g:slist-t (g:boxed attribute :return))
 #+liber-documentation
 "@version{2024-2-27}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @begin{return}
    A list of all @class{pango:attributes} instances in @arg{attrlist}.
  @end{return}
  @begin{short}
    Gets a list of all attributes in @arg{attrlist}.
  @end{short}

  Since 1.44
  @see-class{pango:attr-list}
  @see-class{pango:attribute}"
  (attrlist (g:boxed attr-list)))

#+pango-1-44
(export 'attr-list-attributes)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_from_string
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_attr_list_from_string" attr-list-from-string)
    (g:boxed attr-list :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[text]{a string with the data}
  @return{The newly created @class{pango:attr-list} instance.}
  @begin{short}
    Deserializes a @class{pango:attr-list} instance from a string.
  @end{short}
  This is the counterpart to the @fun{pango:attr-list-to-string} function. See
  that functions for details about the format.

  Since 1.50
  @see-class{pango:attr-list}
  @see-function{pango:attr-list-to-string}"
  (text :string))

#+pango-1-50
(export 'attr-list-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_to_string ()
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_attr_list_to_string" attr-list-to-string) :string
 #+liber-documentation
 "@version{2024-2-27}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @return{The string with the serialized attributes.}
  @begin{short}
    Serializes a @class{pango-attr-list} instance to a string.
  @end{short}
  In the resulting string, serialized attributes are separated by newlines or
  commas. Individual attributes are serialized to a string of the form
  @begin{pre}
START END TYPE VALUE
  @end{pre}
  Where @code{START} and @code{END} are the indices, with -1 being accepted in
  place of @code{MAXUINT}, @code{TYPE} is the nickname of the attribute value
  type, e.g. @code{weight} or @code{stretch}, and the value is serialized
  according to its type:
  @begin{itemize}
    @item{enum values as nick or numeric value}
    @item{boolean values as @em{true} or @em{false}}
    @item{integers and floats as numbers}
    @item{strings as string, optionally quoted}
    @item{font features as quoted string}
    @item{Pango language as string}
    @item{Pango font description as serialized by the
      @fun{pango:font-description-to-string} function}
    @item{@class{pango-color} as serialized by the @fun{pango:color-to-string}
      function}
  @end{itemize}
  To parse the returned value, use the @fun{pango:attr-list-from-string}
  function.

  Note that shape attributes can not be serialized.
  @begin[Examples]{dictionary}
    @begin{pre}
0 10 foreground red, 5 15 weight bold, 0 200 font-desc \"Sans 10\"
0 -1 weight 700
0 100 family Times
    @end{pre}
  @end{dictionary}
  Since 1.50
  @see-class{pango:attr-list}
  @see-function{pango:attr-list-from-string}"
  (attrlist (g:boxed attr-list)))

#+pango-1-50
(export 'attr-list-to-string)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_get_iterator ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_get_iterator" attr-list-iterator)
    (g:boxed attr-iterator :return)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @return{The @class{pango:attr-iterator} instance.}
  @begin{short}
    Create an iterator initialized to the beginning of the attribute list.
  @end{short}
  The @arg{attrlist} argument must not be modified until this iterator is freed.
  @see-class{pango:attr-list}
  @see-class{pango:attr-iterator}"
  (attrlist (g:boxed attr-list)))

(export 'attr-list-iterator)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_copy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_copy" attr-iterator-copy)
    (g:boxed attr-iterator :return)
 #+liber-documentation
 "@version{2024-2-28}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @return{The newly allocated @class{pango:attr-iterator} instance.}
  @short{Copy a @class{pango:attr-iterator} instance.}
  @see-class{pango:attr-iterator}"
  (iterator (g:boxed attr-iterator)))

(export 'attr-iterator-copy)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_next ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_next" attr-iterator-next) :boolean
 #+liber-documentation
 "@version{2024-2-28}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @return{@em{False} if the iterator is at the end of the attribute list,
    otherwise @em{true}.}
  @short{Advance the iterator until the next change of style.}
  @see-class{pango:attr-iterator}"
  (iterator (g:boxed attr-iterator)))

(export 'attr-iterator-next)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_range ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_range" %attr-iterator-range) :void
  (iterator (g:boxed attr-iterator))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun attr-iterator-range (iterator)
 #+liber-documentation
 "@version{2024-2-28}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @begin{return}
    @arg{start} -- an integer with the start range @br{}
    @arg{end} -- an integer with the end range
  @end{return}
  @begin{short}
    Get the range of the current segment.
  @end{short}
  Note that the stored return values are signed, not unsigned like the values
  in the @class{pango:attribute} instance. To deal with this API oversight,
  stored return values that would not fit into a signed integer are clamped to
  @code{G_MAXINT}.
  @see-class{pango:attr-iterator}
  @see-class{pango:attribute}"
  (cffi:with-foreign-objects ((start :int) (end :int))
    (%attr-iterator-range iterator start end)
    (values (cffi:mem-ref start :int)
            (cffi:mem-ref end :int))))

(export 'attr-iterator-range)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_get ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_get" attr-iterator-get)
    (g:boxed attribute)
 #+liber-documentation
 "@version{2024-2-28}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @argument[type]{a @symbol{pango:attr-type} value with the type of attribute
    to find}
  @return{The current @class{pango:attribute} instance of the given type, or
    @code{nil} if no attribute of that type applies to the current location.}
  @begin{short}
    Find the current attribute of a particular type at the iterator location.
  @end{short}
  When multiple attributes of the same type overlap, the attribute whose range
  starts closest to the current location is used.
  @see-class{pango:attr-iterator}
  @see-symbol{pango:attr-type}"
  (iterator (g:boxed attr-iterator))
  (type attr-type))

(export 'attr-iterator-get)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_get_font ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_get_font" %attr-iterator-font) :void
  (iterator (g:boxed attr-iterator))
  (desc (g:boxed font-description))
  (language :pointer)
  (attrs :pointer))

(defun attr-iterator-font (iterator)
 #+liber-documentation
 "@version{2024-2-28}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @begin{return}
    @arg{desc} -- the @class{pango:font-description} instance with the current
      values @br{}
    @arg{language} -- the @class{pango:language} instance with the language tag
      for item, or @code{nil} if none is found @br{}
    @arg{attrs} -- the list of @class{pango:attribute} instances with non-font
      attributes at the the current position, only the highest priority value
      of each attribute will be added to this list
  @end{return}
  @begin{short}
    Get the font and other attributes at the current iterator position.
  @end{short}
  @see-class{pango:attr-iterator}
  @see-class{pango:font-description}
  @see-class{pango:language}
  @see-class{pango:attribute}"
  (let ((desc (pango:font-description-new)))
    (cffi:with-foreign-objects ((language :pointer) (attrs :pointer))
      (%attr-iterator-font iterator desc language attrs)
      (values desc
              (cffi:mem-ref language '(g:boxed language :return))
              (cffi:mem-ref attrs '(g:slist-t (g:boxed attribute :return)))))))

(export 'attr-iterator-font)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_get_attrs ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_get_attrs" attr-iterator-attrs)
    (g:slist-t (g:boxed attribute :return))
 #+liber-documentation
 "@version{2024-2-28}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @return{The list of all @class{pango:attribute} instances for the current
    range.}
  @begin{short}
    Gets a list of all attributes at the current position of the iterator.
  @end{short}
  @see-class{pango:attr-iterator}
  @see-class{pango:attribute}"
  (iterator (g:boxed attr-iterator)))

(export 'attr-iterator-attrs)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_destroy ()
;;;
;;; void pango_attr_iterator_destroy (PangoAttrIterator *iterator);
;;;
;;; Destroy a PangoAttrIterator and free all associated memory.
;;;
;;; iterator :
;;;     a PangoAttrIterator
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.attributes.lisp --------------------------------------
