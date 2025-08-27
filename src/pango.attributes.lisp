;;; ----------------------------------------------------------------------------
;;; pango.attributes.lisp
;;;
;;; The documentation in this file is taken from the Pango Reference Manual
;;; version 1.56 and modified to document the Lisp binding to the Pango
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;;     PANGO_SCALE_XX_SMALL
;;;     PANGO_SCALE_X_SMALL
;;;     PANGO_SCALE_SMALL
;;;     PANGO_SCALE_MEDIUM
;;;     PANGO_SCALE_LARGE
;;;     PANGO_SCALE_X_LARGE
;;;     PANGO_SCALE_XX_LARGE
;;;
;;;     PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING
;;;     PANGO_ATTR_INDEX_TO_TEXT_END
;;;
;;;     PangoAttrType
;;;     PangoUnderline
;;;     PangoOverline
;;;     PangoShowFlags
;;;     PangoTextTransform                                  Since 1.50
;;;     PangoBaseLineShift                                  Since 1.50
;;;     PangoFontScale                                      Since 1.50
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
;;;     pango_attribute_init                                not implemented
;;;     pango_attribute_copy
;;;     pango_attribute_equal
;;;     pango_attribute_destroy                             not needed
;;;
;;;     pango_attr_language_new
;;;     pango_attr_family_new
;;;     pango_attr_style_new
;;;     pango_attr_weight_new
;;;     pango_attr_variant_new
;;;     pango_attr_stretch_new
;;;     pango_attr_size_new
;;;     pango_attr_font_desc_new
;;;     pango_attr_foreground_new
;;;     pango_attr_background_new
;;;     pango_attr_underline_new
;;;     pango_attr_strikethrough_new
;;;     pango_attr_rise_new
;;;     pango_attr_shape_new
;;;     pango_attr_scale_new
;;;     pango_attr_fallback_new
;;;     pango_attr_letter_spacing_new
;;;     pango_attr_underline_color_new
;;;     pango_attr_strikethrough_color_new
;;;     pango_attr_size_new_absolute
;;;     pango_attr_gravity_new
;;;     pango_attr_gravity_hint_new
;;;     pango_attr_font_features_new
;;;     pango_attr_foreground_alpha_new
;;;     pango_attr_background_alpha_new
;;;     pango_attr_allow_breaks_new
;;;     pango_attr_show_new
;;;     pango_attr_insert_hyphens_new
;;;     pango_attr_overline_new
;;;     pango_attr_overline_color_new
;;;     pango_attr_line-height_new                          Since 1.50
;;;     pango_attr_line_height_new_absolute                 Since 1.50
;;;     pango_attr_text_transform_new                       Since 1.50
;;;     pango_attr_word_new                                 Since 1.50
;;;     pango_attr_sentence_new                             Since 1.50
;;;     pango_attr_baseline_shift_new                       Since 1.50
;;;     pango_attr_font_scale_new                           Since 1.50
;;;
;;;     PangoAttrDataCopyFunc                               not implemented
;;;     pango_attr_shape_new_with_data                      not implmeneted
;;;
;;;     PangoAttrFilterFunc
;;;
;;;     pango_attr_list_new
;;;     pango_attr_list_ref                                 not needed
;;;     pango_attr_list_unref                               not needed
;;;     pango_attr_list_copy
;;;     pango_attr_list_equal
;;;     pango_attr_list_insert
;;;     pango_attr_list_insert_before
;;;     pango_attr_list_change
;;;     pango_attr_list_splice
;;;     pango_attr_list_filter
;;;     pango_attr_list_update
;;;     pango_attr_list_get_attributes
;;;     pango_attr_list_from_string                         Since 1.50
;;;     pango_attr_list_to_string                           Since 1.50
;;;     pango_attr_list_get_iterator
;;;
;;;     pango_attr_iterator_copy
;;;     pango_attr_iterator_next
;;;     pango_attr_iterator_range
;;;     pango_attr_iterator_get
;;;     pango_attr_iterator_get_font
;;;     pango_attr_iterator_get_attrs
;;;     pango_attr_iterator_destroy                         not needed
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
;;; PANGO_SCALE_XX_SMALL
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale-xx-small+) "Constant")

(defconstant +scale-xx-small+ #.(/ 1.0d0 (* 1.2d0 1.2d0 1.2d0))
 #+liber-documentation
 "@version{2025-01-01}
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
 "@version{2025-01-01}
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
 "@version{2025-01-01}
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
 "@version{2025-01-01}
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
 "@version{2025-01-01}
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
 "@version{2025-01-01}
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
 "@version{2025-01-01}
  @variable-value{@code{(* 1.2d0 1.2d0 1.2d0)}}
  @begin{short}
    The scale factor for three magnification steps.
  @end{short}")

(export '+scale-xx-large+)

;;; ----------------------------------------------------------------------------
;;; PANGO_ATTR_INDEX_FROM_TEXT_BEGINNING
;;; ----------------------------------------------------------------------------

(defconstant +attr-index-from-text-beginning+ 0
 #+liber-documentation
 "@version{2025-08-17}
  @variable-value{0}
  @begin{short}
    Value for the start index in the @class{pango:attribute} instance that
    indicates the beginning of the text.
  @end{short}
  @see-class{pango:attribute}")

(export '+attr-index-from-text-beginning+)

;;; ----------------------------------------------------------------------------
;;; PANGO_ATTR_INDEX_TO_TEXT_END
;;; ----------------------------------------------------------------------------

(defconstant +attr-index-to-text-end+ 4294967295
 #+liber-documentation
 "@version{2025-08-17}
  @variable-value{4294967295}
  @begin{short}
    Value for the end index in the @class{pango:attribute} instance that
    indicates the end of the text.
  @end{short}
  @see-class{pango:attribute}")

(export '+attr-index-to-text-end+)

;;; ----------------------------------------------------------------------------
;;; PangoAttrType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoAttrType" attr-type
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
  (:allow-breaks 26)
  (:show 27)
  (:insert-hyphens 28)
  (:overline 29)
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
 "@version{2025-08-16}
  @begin{declaration}
(gobject:define-genum \"PangoAttrType\" attr-type
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
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:invalid]{Does not happen.}
      @entry[:language]{Language.}
      @entry[:family]{Font family name list.}
      @entry[:style]{Font slant style.}
      @entry[:weight]{Font weight.}
      @entry[:variant]{Font variant, normal or small caps.}
      @entry[:stretch]{Font stretch.}
      @entry[:size]{Font size in points scaled by the @var{pango:+scale+}
        value.}
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
      @entry[:allow-breaks]{Whether breaks are allowed.}
      @entry[:show]{How to render invisible characters.}
      @entry[:insert-hyphens]{Whether to insert hyphens at intra-word line
        breaks.}
      @entry[:overline]{Whether the text has an overline.}
      @entry[:overline-color]{Overline color.}
      @entry[:line-height]{Line height factor. Since 1.50}
      @entry[:absolute-line-height]{Line height. Since 1.50}
      @entry[:text-transform]{How Pango treats characters during shaping.
        Since 1.50}
      @entry[:word]{Override segmentation to classify the range of the
        attribute as a single word. Since 1.50}
      @entry[:sentence]{Override segmentation to classify the range of the
        attribute as a single sentence. Since 1.50}
      @entry[:baseline-shift]{Baseline displacement. Since 1.50}
      @entry[:font-scale]{Font-relative size change. Since 1.50}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{pango:attr-type} enumeration distinguishes between different types
    of attributes.
  @end{short}
  Along with the predefined values, it is possible to allocate additional
  values for custom attributes using the @fun{pango:attr-type-register}
  function.
  @see-class{pango:attribute}
  @see-class{pango:attr-list}
  @see-function{pango:attr-type-register}")

;;; ----------------------------------------------------------------------------
;;; PangoUnderline
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoUnderline" underline
  (:export t
   :type-initializer "pango_underline_get_type")
  (:none 0)
  (:single 1)
  (:double 2)
  (:low 3)
  (:error 4)
  (:single-line 5)
  (:double-line 6)
  (:error-line 7))

#+liber-documentation
(setf (liber:alias-for-symbol 'underline)
      "GEnum"
      (liber:symbol-documentation 'underline)
 "@version{2025-08-16}
  @begin{declaration}
(gobject:define-genum \"PangoUnderline\" underline
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
  @end{declaration}
  @begin{values}
  @begin[code]{simple-table}
      @entry[:none]{No underline should be drawn.}
      @entry[:single]{A single underline should be drawn.}
      @entry[:double]{A double underline should be drawn.}
      @entry[:low]{A single underline should be drawn at a position beneath the
        ink extents of the text being underlined. This should be used only for
        underlining single characters, such as for keyboard accelerators. The
        @val[pango:underline]{:single} value should be used for extended
        portions of text.}
      @entry[:error]{A wavy underline should be drawn below. This underline is
        typically used to indicate an error such as a possible mispelling. In
        some cases a contrasting color may automatically be used.}
      @entry[:single-line]{Like the @val[pango:underline]{:single} value, but
        drawn continuously across multiple runs.}
      @entry[:double-line]{Like the @val[pango:underline]{:double} value, but
        drawn continuously across multiple runs.}
      @entry[:error-line]{Like the @val[pango:underline]{:error} value, but
        drawn continuously across multiple runs.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{pango:underline} enumeration is used to specify whether text should
    be underlined, and if so, the type of underlining.
  @end{short}
  @see-symbol{pango:attribute}
  @see-symbol{pango:overline}
  @see-function{pango:attr-underline-new}")

;;; ----------------------------------------------------------------------------
;;; PangoOverline
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoOverline" overline
  (:export t
   :type-initializer "pango_overline_get_type")
  (:none 0)
  (:single 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'overline)
      "GEnum"
      (liber:symbol-documentation 'overline)
 "@version{2025-08-16}
  @begin{declaration}
(gobject:define-genum \"PangoOverline\" overline
  (:export t
   :type-initializer \"pango_overline_get_type\")
  (:none 0)
  (:single 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{No overline should be drawn.}
      @entry[:single]{Draw a single line above the ink extents of the text
        being overlined.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{pango:overline} enumeration is used to specify whether text should
    be overlined, and if so, the type of line.
  @end{short}
  @see-symbol{pango:attribute}
  @see-symbol{pango:underline}
  @see-function{pango:attr-overline-new}")

;;; ----------------------------------------------------------------------------
;;; PangoShowFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "PangoShowFlags" show-flags
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
 "@version{2025-08-16}
  @begin{declaration}
(gobject:define-gflags \"PangoShowFlags\" show-flags
  (:export t
   :type-initializer \"pango_show_flags_get_type\")
  (:none 0)
  (:spaces #.(ash 1 0))
  (:line-breaks #.(ash 1 1))
  (:ignorables #.(ash 1 2)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{No special treatment for invisible characters.}
      @entry[:spaces]{Render spaces, tabs and newlines visibly.}
      @entry[:line-breaks]{Render line breaks visibly.}
      @entry[:ignorables]{Render default-ignorable Unicode characters visibly.}
    @end{simple-table}
  @end{values}
  @begin{short}
    These flags affect how Pango treats characters that are normally not
    visible in the output.
  @end{short}
  @see-symbol{pango:attribute}
  @see-function{pango:attr-show-new}")

;;; ----------------------------------------------------------------------------
;;; PangoTextTransform
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoTextTransform" text-transform
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
 "@version{2025-08-16}
  @begin{declaration}
(gobject:define-genum \"PangoTextTransform\" text-transform
  (:export t
   :type-initializer \"pango_text_transform_get_type\")
  :none
  :lowercase
  :uppercase
  :capitalize)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{Leave text unchanged.}
      @entry[:lowercase]{Display letters and numbers as lowercase.}
      @entry[:uppercase]{Display letters and numbers as uppercase.}
      @entry[:capitalize]{Display the first character of a word in titlecase.}
    @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration that affects how Pango treats characters during shaping.
  @end{short}
  Since 1.50
  @see-symbol{pango:attribute}")

;;; ----------------------------------------------------------------------------
;;; PangoBaselineShift
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoBaselineShift" baseline-shift
  (:export t
   :allow-undeclared-values t
   :type-initializer "pango_baseline_shift_get_type")
  :none
  :superscript
  :subscript)

#+liber-documentation
(setf (liber:alias-for-symbol 'baseline-shift)
      "GEnum"
      (liber:symbol-documentation 'baseline-shift)
 "@version{2025-08-16}
  @begin{declaration}
(gobject:define-genum \"PangoBaselineShift\" baseline-shift
  (:export t
   :type-initializer \"pango_baseline_shift_get_type\")
  :none
  :superscript
  :subscript)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{Leave the baseline unchanged.}
      @entry[:superscript]{Shift the baseline to the superscript position,
        relative to the previous run.}
      @entry[:subscript]{Shift the baseline to the subscript position, relative
        to the previous run.}
    @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration that affects baseline shifts between runs.
  @end{short}
  Since 1.50
  @see-symbol{pango:attribute}")

;;; ----------------------------------------------------------------------------
;;; PangoFontScale
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoFontScale" font-scale
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
 "@version{2025-08-16}
  @begin{declaration}
(gobject:define-genum \"PangoFontScale\" font-scale
  (:export t
   :type-initializer \"pango_font_scale_get_type\")
  :none
  :superscript
  :subscript
  :small-caps)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{Leave the font size unchanged.}
      @entry[:superscript]{Change the font to a size suitable for superscripts.}
      @entry[:subscript]{Change the font to a size suitable for subscripts.}
      @entry[:small-caps]{Change the font to a size suitable for Small Caps.}
    @end{simple-table}
  @end{values}
  @begin{short}
    An enumeration that affects font sizes for superscript and subscript
    positioning and for (emulated) Small Caps.
  @end{short}
  Since 1.50
  @see-symbol{pango:attribute}")

;;; ----------------------------------------------------------------------------
;;; PangoAttrClass                                          not exported
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %attr-class
  (type attr-type)
  (copy :pointer)
  (destroy :pointer)
  (equal :pointer))

;;; ----------------------------------------------------------------------------
;;; PangoAttribute
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %attribute
  (klass :pointer)
  (start-index :uint)
  (end-index :uint))

(glib:define-gboxed-opaque attribute "PangoAttribute"
   :export t
   :type-initializer "pango_attribute_get_type"
   :alloc (error "PangoAttribute cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'attribute)
      "GBoxed"
      (documentation 'attribute 'type)
 "@version{2025-08-17}
  @begin{declaration}
(glib:define-gboxed-opaque attribute \"PangoAttribute\"
   :export t
   :type-initializer \"pango_attribute_get_type\"
   :alloc (error \"PangoAttribute cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The @class{pango:attribute} structure represents the common portions of all
    attributes.
  @end{short}
  Particular types of attributes include this structure as their initial
  portion. The common portion of the attribute holds the range to which the
  value in the type specific part of the attribute applies. By default an
  attribute will have an all inclusive range from
  @var{pango:+attr-index-from-text-beginning+} to
  @var{pango:+attr-index-to-text-end+}.
  @see-constructor{pango:attribute-new}
  @see-constructor{pango:attribute-copy}
  @see-slot{pango:attribute-start-index}
  @see-slot{pango:attribute-end-index}
  @see-slot{pango:attribute-type}
  @see-symbol{pango:attr-type}")

;;; --- pango:attribute-type ---------------------------------------------------

;; Get the type of the PangoAttribute from the KLASS slot

(defun attribute-type (attribute)
  (let* ((ptr (glib:boxed-opaque-pointer attribute))
         (klass (cffi:foreign-slot-value ptr
                                         '(:struct %attribute)
                                         'klass)))
    (cffi:foreign-slot-value klass '(:struct %attr-class) 'type)))

#+liber-documentation
(setf (liber:alias-for-function 'attribute-type)
      "Accessor"
      (documentation 'attribute-type 'function)
 "@version{2025-08-16}
  @syntax{(pango:attribute-type attribute) => type}
  @argument[attribute]{a @class{pango:attribute} instance}
  @argument[type]{a @sym{pango:attr-type} value}
  @begin{short}
    The accessor for the @arg{type} slot of the @class{pango:attribute}
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
 "@version{2025-08-16}
  @syntax{(pango:attribute-start-index attribute) => index}
  @syntax{(setf (pango:attribute-start-index attribute) index)}
  @argument[attribute]{a @class{pango:attribute} instance}
  @argument[index]{an integer for the start index}
  @begin{short}
    The accessor for the @arg{start-index} slot of the @class{pango:attribute}
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
 "@version{2025-08-16}
  @syntax{(pango:attribute-end-index attribute) => index}
  @syntax{(setf (pango:attribute-end-index attribute) index)}
  @argument[attribute]{a @class{pango:attribute} instance}
  @argument[index]{an integer for the end index}
  @begin{short}
    The accessor for the @arg{end-index} slot of the @class{pango:attribute}
    structure.
  @end{short}
  @see-class{pango:attribute}")

(export 'attribute-end-index)

;;; ----------------------------------------------------------------------------
;;; pango_attribute_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attribute_copy" attribute-copy)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[attribute]{a @class{pango:attribute} instance}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Makes a copy of an attribute.}
  @see-class{pango:attribute}"
  (attribute (g:boxed attribute)))

(export 'attribute-copy)

;;; ----------------------------------------------------------------------------
;;; pango_attribute_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attribute_equal" attribute-equal) :boolean
 #+liber-documentation
 "@version{2025-01-01}
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
;;; pango_attribute_destroy                                 not needed
;;;
;;; Destroy a PangoAttribute and free all associated memory.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_type_register
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_type_register" attr-type-register) attr-type
 #+liber-documentation
 "@version{2025-08-24}
  @argument[name]{a string for an identifier for the attribute type}
  @return{The integer for the new type ID.}
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
;;; pango_attr_type_get_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_type_get_name" attr-type-name)
    (:string :free-from-foreign nil)
  #+liber-documentation
  "@version{2025-08-17}
   @argument[type]{an integer for an attribute type ID to fetch the name for}
   @begin{return}
    The string for the type ID name, or @code{nil} if @arg{type} is a built-in
    Pango attribute type or invalid.
  @end{return}
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
;;; pango:attribute-new
;;; ----------------------------------------------------------------------------

;; TODO: Consider to improve the implementation with default arguments for
;; the start and end index

(defun attribute-new (type start end &rest args)
 #+liber-documentation
 "@version{2025-08-17}
  @argument[type]{a @sym{pango:attr-type} value}
  @argument[start]{an integer for the start index}
  @argument[end]{an integer for the end index}
  @argument[args]{arguments for the constructor of a @arg{type} attribute}
  @begin{short}
    Creates an attribute for @arg{type}. Initializes the start index with
    @arg{start} and the end index with @arg{end}.
  @end{short}
  Pass the @var{pango:+attr-index-from-text-beginning+} value for @arg{start}
  and @var{pango:+attr-index-to-text-end+} value for @arg{end} to cover the
  possible text range.
  @begin[Notes]{dictionary}
  This is a Lisp utility function, which calls the corresponding
  @sym{pango:attr-<type>-new} constructor for the given @arg{type}, for
  example the @fun{pango:attr-weight-new} constructor for the
  @val[pango:attr-type]{:weight} attribute type.
  @end{dictionary}
  @see-class{pango:attribute}"
  (let ((attribute (cond ((eq :language type)
                          (apply #'attr-language-new args))
                         ((eq :family type)
                          (apply #'attr-family-new args))
                         ((eq :style type)
                          (apply #'attr-style-new args))
                         ((eq :weight type)
                          (apply #'attr-weight-new args))
                         ((eq :variant type)
                          (apply #'attr-variant-new args))
                         ((eq :stretch type)
                          (apply #'attr-stretch-new args))
                         ((eq :size type)
                          (apply #'attr-size-new args))
                         ((eq :font-desc type)
                          (apply #'attr-font-desc-new args))
                         ((eq :foreground type)
                          (apply #'attr-foreground-new args))
                         ((eq :background type)
                          (apply #'attr-background-new args))
                         ((eq :underline type)
                          (apply #'attr-underline-new args))
                         ((eq :strikethrough type)
                          (apply #'attr-strikethrough-new args))
                         ((eq :rise type)
                          (apply #'attr-rise-new args))
                         ((eq :shape type)
                          (apply #'attr-shape-new args))
                         ((eq :scale type)
                          (apply #'attr-scale-new args))
                         ((eq :fallback type)
                          (apply #'attr-fallback-new args))
                         ((eq :letter-spacing type)
                          (apply #'attr-letter-spacing-new args))
                         ((eq :underline-color type)
                          (apply #'attr-underline-color-new args))
                         ((eq :strikethrough-color type)
                          (apply #'attr-strikethrough-color-new args))
                         ((eq :absolute-size type)
                          (apply #'attr-size-new-absolute args))
                         ((eq :gravity type)
                          (apply #'attr-gravity-new args))
                         ((eq :gravity-hint type)
                          (apply #'attr-gravity-hint-new args))
                         ((eq :font-features type)
                          (apply #'attr-font-features-new args))
                         ((eq :foreground-alpha type)
                          (apply #'attr-foreground-alpha-new args))
                         ((eq :background-alpha type)
                          (apply #'attr-background-alpha-new args))
                         ((eq :allow-breaks type)
                          (apply #'attr-allow-breaks-new args))
                         ((eq :show type)
                          (apply #'attr-show-new args))
                         ((eq :insert-hyphens type)
                          (apply #'attr-insert-hyphens-new args))
                         ((eq :overline type)
                          (apply #'attr-overline-new args))
                         ((eq :overline-color type)
                          (apply #'attr-overline-color-new args))
                         ((eq :line-height type)
                          (apply #'attr-line-height-new args))
                         ((eq :absolute-line-height type)
                          (apply #'attr-line-height-new-absolute args))
                         ((eq :text-transform type)
                          (apply #'attr-text-transform-new args))
                         ((eq :word type)
                          (apply #'attr-word-new args))
                         ((eq :sentence type)
                          (apply #'attr-sentence-new args))
                         ((eq :baseline-shift type)
                          (apply #'attr-baseline-shift-new args))
                         ((eq :font-scale type)
                          (apply #'attr-font-scale-new args))
                         (t
                          (error "PANGO:ATTR-NEW: Unknown type ~a" type)))))
    (setf (attribute-start-index attribute) start)
    (setf (attribute-end-index attribute) end)
    attribute))

(export 'attribute-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_language_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_language_new" attr-language-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[language]{a @class{pango:language} instance}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new language tag attribute.}
  @see-class{pango:attribute}
  @see-class{pango:language}"
  (language (g:boxed language)))

(export 'attr-language-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_family_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_family_new" attr-family-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[family]{a string for the family or comma separated list of
    families}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font family attribute.}
  @see-class{pango:attribute}"
  (family :string))

(export 'attr-family-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_style_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_style_new" attr-style-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[style]{a @sym{pango:style} value for the slant style}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font slant style attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:style}"
  (style style))

(export 'attr-style-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_weight_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_weight_new" attr-weight-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[weight]{a @sym{pango:weight} value for the weight}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font weight attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:weight}"
  (weight weight))

(export 'attr-weight-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_variant_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_variant_new" attr-variant-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[variant]{a @sym{pango:variant} value for the variant}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font variant attribute (normal or small caps).}
  @see-class{pango:attribute}
  @see-symbol{pango:variant}"
  (variant variant))

(export 'attr-variant-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_stretch_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_stretch_new" attr-stretch-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[stretch]{a @sym{pango:stretch} value for the stretch}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font stretch attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:stretch}"
  (stretch stretch))

(export 'attr-stretch-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_size_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_size_new" attr-size-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[size]{an integer for the font size, in @var{pango:+scale+} units
    of a point}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font-size attribute in fractional points.}
  @see-class{pango:attribute}"
  (size :int))

(export 'attr-size-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_font_desc_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_font_desc_new" attr-font-desc-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-01-01}
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
;;; pango_attr_foreground_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_foreground_new" attr-foreground-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[red]{an integer for the red value, ranging from 0 to 65535}
  @argument[green]{an integer for the green value}
  @argument[blue]{an integer for the blue value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new foreground color attribute.}
  @see-class{pango:attribute}"
  (red :uint16)
  (green :uint16)
  (blue :uint16))

(export 'attr-foreground-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_background_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_background_new" attr-background-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[red]{an integer for the red value, ranging from 0 to 65535}
  @argument[green]{an integer for the green value}
  @argument[blue]{an integer for the blue value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new background color attribute.}
  @see-class{pango:attribute}"
  (red :uint16)
  (green :uint16)
  (blue :uint16))

(export 'attr-background-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_underline_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_underline_new" attr-underline-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[underline]{a @sym{pango:underline} value for the underline style}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new underline style attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:underline}"
  (underline underline))

(export 'attr-underline-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_strikethrough_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_strikethrough_new" attr-strikethrough-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[strikethrough]{@em{true} if the text should be struck-through}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new strikethrough attribute.}
  @see-class{pango:attribute}"
  (strikethrough :boolean))

(export 'attr-strikethrough-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_rise_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_rise_new" attr-rise-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[rise]{an integer for the amount that the text should be displaced
    vertically, in Pango units, positive values displace the text upwards}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new baseline displacement attribute.}
  @see-class{pango:attribute}"
  (rise :int))

(export 'attr-rise-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_shape_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_shape_new" attr-shape-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[ink]{a @sym{pango:rectangle} instance to assign to each character}
  @argument[logical]{a @sym{pango:rectangle} instance to assign to each
    character}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Creates a new shape attribute.
  @end{short}
  A shape is used to impose a particular ink and logical rectangle on the result
  of shaping a particular glyph. This might be used, for instance, for embedding
  a picture or a widget inside a Pango layout.
  @see-class{pango:attribute}
  @see-symbol{pango:rectangle}"
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(export 'attr-shape-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_scale_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_scale_new" %attr-scale-new)
    (g:boxed attribute :return)
  (factor :double))

(defun attr-scale-new (factor)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[factor]{a number coerced to a double float for the factor to scale
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
;;; pango_attr_fallback_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_fallback_new" attr-fallback-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
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
  (fallback :boolean))

(export 'attr-fallback-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_letter_spacing_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_letter_spacing_new" attr-letter-spacing-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[spacing]{an integer for the amount of extra space to add between
    graphemes of the text, in Pango units}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new letter spacing attribute.}
  @see-class{pango:attribute}"
  (spacing :int))

(export 'attr-letter-spacing-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_underline_color_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_underline_color_new" attr-underline-color-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[red]{an integer for the red value, ranging from 0 to 65535}
  @argument[green]{an integer for the green value}
  @argument[blue]{an integer for the blue value}
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
;;; pango_attr_strikethrough_color_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_strikethrough_color_new"
                attr-strikethrough-color-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[red]{an integer for the red value, ranging from 0 to 65535}
  @argument[green]{an integer for the green value}
  @argument[blue]{an integer for the blue value}
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
;;; pango_attr_size_new_absolute
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_size_new_absolute" attr-size-new-absolute)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[size]{an integer for the font size, in @var{pango:+scale+} units
    of device units}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Creates a new font-size attribute in device units.}
  @see-class{pango:attribute}"
  (size :int))

(export 'attr-size-new-absolute)

;;; ----------------------------------------------------------------------------
;;; pango_attr_gravity_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_gravity_new" attr-gravity-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[gravity]{a @sym{pango:gravity} value, should not be
    @val[pango:gravity]{:auto}}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new gravity attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:gravity}"
  (gravity gravity))

(export 'attr-gravity-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_gravity_hint_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_gravity_hint_new" attr-gravity-hint-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[hint]{a @sym{pango:gravity-hint} value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new gravity hint attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:gravity-hint}"
  (hint gravity-hint))

(export 'attr-gravity-hint-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_font_features_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_font_features_new" attr-font-features-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[features]{a string for OpenType font features, in CSS syntax}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new font features tag attribute.}
  @see-class{pango:attribute}"
  (features :string))

(export 'attr-font-features-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_foreground_alpha_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_foreground_alpha_new" attr-foreground-alpha-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[alpha]{an unsigned integer for the alpha value, between 1 and 65536}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new foreground alpha attribute.}
  @see-class{pango:attribute}"
  (alpha :uint16))

(export 'attr-foreground-alpha-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_background_alpha_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_background_alpha_new" attr-background-alpha-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[alpha]{an unsigned integer for the alpha value, between 1 and 65536}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new background alpha attribute.}
  @see-class{pango:attribute}"
  (alpha :uint16))

(export 'attr-background-alpha-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_allow_breaks_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_allow_breaks_new" attr-allow-breaks-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-05-02}
  @argument[allow]{@em{true} if line breaks are allowed}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new allow breaks attribute.
  @end{short}
  If breaks are disabled, the range will be kept in a single run, as far as
  possible.
  @see-class{pango:attribute}"
  (allow :boolean))

(export 'attr-allow-breaks-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_show_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_show_new" attr-show-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[flags]{a @sym{pango:show-flags} value to apply.}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new attribute that influences how invisible characters are
    rendered.
  @end{short}
  @see-class{pango:attribute}
  @see-symbol{pango:show-flags}"
  (flags show-flags))

(export 'attr-show-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_insert_hyphens_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_insert_hyphens_new" attr-insert-hyphens-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-05-02}
  @argument[hyphens]{@em{true} if hyphens should be inserted}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new insert hyphens attribute.
  @end{short}
  Pango will insert hyphens when breaking lines in the middle of a word. This
  attribute can be used to suppress the hyphen.
  @see-class{pango:attribute}"
  (insert :boolean))

(export 'attr-insert-hyphens-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_overline_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_overline_new" attr-overline-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[overline]{a @sym{pango:overline} value for the overline style}
  @return{The newly allocated @class{pango:attribute} instance.}
  @short{Create a new overline style attribute.}
  @see-class{pango:attribute}
  @see-symbol{pango:overline}"
  (overline overline))

(export 'attr-overline-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_overline_color_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_overline_color_new" attr-overline-color-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-05-02}
  @argument[red]{an integer for the red value, ranging from 0 to 65535}
  @argument[green]{an integer for the green value}
  @argument[blue]{an integer for the blue value}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new overline color attribute.
  @end{short}
  This attribute modifies the color of overlines. If not set, overlines will
  use the foreground color.
  @see-class{pango:attribute}"
  (red :uint16)
  (green :uint16)
  (blue :uint16))

(export 'attr-overline-color-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_line-height_new                              Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_line_height_new" %attr-line-height-new)
    (g:boxed attribute :return)
 (factor :double))

(defun attr-line-height-new (factor)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[factor]{a number coerced to a double float for the scaling factor
    to apply to the logical height}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Modifies the height of logical line extents by a factor.
  @end{short}

  This affects the values returned by the @fun{pango:layout-line-extents},
  @fun{pango:layout-line-pixel-extents} and @fun{pango:layout-iter-line-extents}
  functions.

  Since 1.50
  @see-class{pango:attribute}
  @see-function{pango:layout-line-extents}
  @see-function{pango:layout-line-pixel-extents}
  @see-function{pango:layout-iter-line-extents}"
  (%attr-line-height-new (coerce factor 'double-float)))

(export 'attr-line-height-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_line_height_new_absolute                     Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_line_height_new_absolute"
               attr-line-height-new-absolute) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[height]{an integer for the line height, in @var{pango:+scale+}}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Modifies the height of logical line extents to be @arg{height}.
  @end{short}

  This affects the values returned by the @fun{pango:layout-line-extents},
  @fun{pango:layout-line-pixel-extents} and @fun{pango:layout-iter-line-extents}
  functions.

  Since 1.50
  @see-class{pango:attribute}
  @see-function{pango:layout-line-extents}
  @see-function{pango:layout-line-pixel-extents}
  @see-function{pango:layout-iter-line-extents}"
  (height :int))

(export 'attr-line-height-new-absolute)

;;; ----------------------------------------------------------------------------
;;; pango_attr_text_transform_new                           Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_text_transform_new" attr-text-transform-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[transform]{a @sym{pango:text-transform} value to apply}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Creates a new attribute that influences how characters are transformed
    during shaping.
  @end{short}

  Since 1.50
  @see-class{pango:attribute}
  @see-symbol{pango:text-transform}"
  (transform text-transform))

(export 'attr-text-transform-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_word_new                                     Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_word_new" attr-word-new) (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Marks the range of the attribute as a single word.
  @end{short}
  Note that this may require adjustments to word and sentence classification
  around the range.

  Since 1.50
  @see-class{pango:attribute}")

(export 'attr-word-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_sentence_new                                 Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_sentence_new" attr-sentence-new)
    (g:boxed attribute :return)
#+liber-documentation
 "@version{2025-08-16}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Marks the range of the attribute as a single sentence.
  @end{short}
  Note that this may require adjustments to word and sentence classification
  around the range.

  Since 1.50
  @see-class{pango:attribute}")

(export 'attr-sentence-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_baseline_shift_new                           Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_baseline_shift_new" attr-baseline-shift-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[shift]{either a @sym{pango:baseline-shift} value or an integer for
    the absolute value in Pango units, relative to the baseline of the previous
    run, positive values displace the text upwards}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Creates a new baseline displacement attribute.
  @end{short}
  The effect of this attribute is to shift the baseline of a run, relative to
  the run of preceding run.

  Since 1.50
  @see-class{pango:attribute}
  @see-symbol{pango:baseline-shift}"
  (shift :int))

(export 'attr-baseline-shift-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_font_scale_new                               Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_font_scale_new" attr-font-scale-new)
    (g:boxed attribute :return)
 #+liber-documentation
 "@version{2025-08-16}
  @argument[scale]{a @sym{pango:font-scale} value, which indicates font size
    change relative to the size of the previous run}
  @return{The newly allocated @class{pango:attribute} instance.}
  @begin{short}
    Create a new font scale attribute.
  @end{short}
  The effect of this attribute is to change the font size of a run, relative to
  the size of preceding run.

  Since 1.50
  @see-class{pango:attribute}
  @see-symbol{pango:font-scale}"
  (scale font-scale))

(export 'attr-font-scale-new)

;;; ----------------------------------------------------------------------------
;;; PangoAttrDataCopyFunc
;;;
;;; A copy function passed to attribute new functions that take user data.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_shape_new_with_data
;;;
;;; Like pango_attr_shape_new(), but a user data pointer is also provided; this
;;; pointer can be accessed when later rendering the glyph.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoAttrIterator
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque attr-iterator "PangoAttrIterator"
  :export t
  :type-initializer "pango_attr_iterator_get_type"
  :alloc (error "PangoAttrIterator cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'attr-iterator)
      "GBoxed"
      (documentation 'attr-iterator 'type)
 "@version{2025-01-01}
  @begin{declaration}
(glib:define-gboxed-opaque attr-iterator \"PangoAttrIterator\"
  :export t
  :type-initializer \"pango_attr_iterator_get_type\"
  :alloc (error \"PangoAttrIterator cannot be created from the Lisp side.\"))
  @end{declaration}
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

(glib:define-gboxed-opaque attr-list "PangoAttrList"
  :type-initializer "pango_attr_list_get_type"
  :alloc (%attr-list-new))

#+liber-documentation
(setf (liber:alias-for-class 'attr-list)
      "GBoxed"
      (documentation 'attr-list 'type)
 "@version{2025-01-01}
  @begin{declaration}
(glib:define-gboxed-opaque attr-list \"PangoAttrList\"
  :type-initializer \"pango_attr_list_get_type\"
  :alloc (%attr-list-new))
  @end{declaration}
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
  @see-constructor{pango:attr-list-new}
  @see-constructor{pango:attr-list-copy}
  @see-class{pango:attr-iterator}
  @see-function{pango:attr-list-change}")

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_new" %attr-list-new) :pointer)

(cffi:defcfun ("pango_attr_list_new" attr-list-new)
    (g:boxed attr-list :return)
 #+liber-documentation
 "@version{2025-01-01}
  @return{The newly allocated @class{pango:attr-list} instance.}
  @begin{short}
    Create a new empty attribute list.
  @end{short}
  @see-class{pango:attr-list}")

(export 'attr-list-new)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_ref
;;;
;;; Increase the reference count of the given attribute list by one.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_unref
;;;
;;; Decrease the reference count of the given attribute list by one.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_copy" attr-list-copy)
    (g:boxed attr-list :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[attrlist]{a @class{pango:attr-list} instance, may be @code{nil}}
  @begin{return}
    The newly allocated @class{pango:attr-list} instance. Returns @code{nil}
    if @arg{attr-list} is @code{nil}.
  @end{return}
  @begin{short}
    Copy @arg{attrlist} and return an identical new attribute list.
  @end{short}
  @see-class{pango:attr-list}"
  (attrlist (g:boxed attr-list)))

(export 'attr-list-copy)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_equal" attr-list-equal) :boolean
 #+liber-documentation
 "@version{2025-05-02}
  @argument[attrlist1]{a @class{pango:attr-list} instance}
  @argument[attrlist2]{a @class{pango:attr-list} instance}
  @begin{short}
    Checks whether @arg{listattr1} and @arg{listattr2} contain the same
    attributes and whether those attributes apply to the same ranges.
  @end{short}
  Beware that this will return wrong values if any list contains duplicates.
  @see-class{pango:attr-list}"
  (attrlist1 (g:boxed attr-list))
  (attrlist2 (g:boxed attr-list)))

(export 'attr-list-equal)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_insert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_insert" attr-list-insert) :void
 #+liber-documentation
 "@version{2025-01-01}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[attribute]{a @class{pango:attribute} instance}
  @begin{short}
    Insert the given attribute into the @class{pango:attr-list} instance.
  @end{short}
  It will be inserted after all other attributes with a matching start index.
  @see-class{pango:attr-list}
  @see-class{pango:attribute}"
  (attrlist (g:boxed attr-list))
  (attribute (g:boxed attribute)))

(export 'attr-list-insert)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_insert_before
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_insert_before" attr-list-insert-before) :void
 #+liber-documentation
 "@version{2025-01-01}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[attribute]{a @class{pango:attribute} instance to insert}
  @begin{short}
    Insert the given attribute into the @class{pango:attr-list} instance.
  @end{short}
  It will be inserted before all other attributes with a matching start index.
  @see-class{pango:attr-list}
  @see-class{pango:attribute}"
  (attrlist (g:boxed attr-list))
  (attribute (g:boxed attribute)))

(export 'attr-list-insert-before)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_change
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_change" attr-list-change) :void
 #+liber-documentation
 "@version{2025-01-01}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[attribute]{a @class{pango:attribute} instance}
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
  (attribute (g:boxed attribute)))

(export 'attr-list-change)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_splice
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_splice" attr-list-splice) :void
 #+liber-documentation
 "@version{#2025-01-01}
  @argument[attrlist1]{a @class{pango:attr-list} instance}
  @argument[attrlist2]{a @class{pango:attr-list} instance}
  @argument[pos]{an integer for the position in the attribute list at which
    to insert @arg{attrlist2}}
  @argument[len]{an integer for the length of the spliced segment, note that
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
  attribute in @arg{attrlist2} in sequence, offset in position by @arg{pos}.

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
;;; PangoAttrFilterFunc
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
 "@version{#2025-01-01}
  @syntax{lambda (attribute) => result}
  @argument[attribute]{a @class{pango:attribute} instance}
  @argument[result]{a boolean whether the attribute should be filtered out}
  @begin{short}
    A predicate callback function used by the @fun{pango:attr-list-filter}
    function to filter out a subset of attributes for an attribute list.
  @end{short}
  @see-class{pango:attribute}
  @see-function{pango:attr-list-filter}")

(export 'attr-filter-func)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_filter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_filter" %attr-list-filter)
    (g:boxed attr-list :return)
  (attrlist (g:boxed attr-list))
  (func :pointer)
  (data :pointer))

(defun attr-list-filter (attrlist func)
 #+liber-documentation
 "@version{#2025-08-17}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[func]{a @sym{pango:attr-filter-func} callback function, returns
    @em{true} if an attribute should be filtered out}
  @begin{return}
    The new @class{pango:attr-list} instance or @code{nil} if no attributes of
    the given types were found.
  @end{return}
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
;;; pango_attr_list_update
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_update" attr-list-update) :void
 #+liber-documentation
 "@version{#2025-05-02}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @argument[pos]{an integer for the position of the change}
  @argument[remove]{an integer for the number of removed bytes}
  @argument[add]{an integer for the number of added bytes}
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
  @see-class{pango:attr-list}"
  (attrlist (g:boxed attr-list))
  (pos :int)
  (remove :int)
  (add :int))

(export 'attr-list-update)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_get_attributes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_get_attributes" attr-list-attributes)
    (g:slist-t (g:boxed attribute :return))
 #+liber-documentation
 "@version{2025-05-02}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @begin{return}
    The list of all @class{pango:attribute} instances in @arg{attrlist}.
  @end{return}
  @begin{short}
    Gets a list of all attributes in @arg{attrlist}.
  @end{short}
  @see-class{pango:attr-list}
  @see-class{pango:attribute}"
  (attrlist (g:boxed attr-list)))

(export 'attr-list-attributes)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_from_string
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_attr_list_from_string" attr-list-from-string)
    (g:boxed attr-list :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[text]{a string for the data}
  @return{The newly created @class{pango:attr-list} instance.}
  @begin{short}
    Deserializes a @class{pango:attr-list} instance from a string.
  @end{short}
  This is the counterpart to the @fun{pango:attr-list-to-string} function. See
  that function for details about the format.

  Since 1.50
  @see-class{pango:attr-list}
  @see-function{pango:attr-list-to-string}"
  (text :string))

#+pango-1-50
(export 'attr-list-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_to_string
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_attr_list_to_string" attr-list-to-string) :string
 #+liber-documentation
 "@version{2025-08-24}
  @argument[attrlist]{a @class{pango:attr-list} instance}
  @return{The string for the serialized attributes.}
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
  type, for example @code{weight} or @code{stretch}, and the value is serialized
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
    @item{@class{pango:color} instances as serialized by the
      @fun{pango:color-to-string} function}
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
  @see-class{pango:color}
  @see-function{pango:attr-list-from-string}
  @see-function{pango:font-description-to-string}
  @see-function{pango:color-to-string}"
  (attrlist (g:boxed attr-list)))

#+pango-1-50
(export 'attr-list-to-string)

;;; ----------------------------------------------------------------------------
;;; pango_attr_list_get_iterator
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_list_get_iterator" attr-list-iterator)
    (g:boxed attr-iterator :return)
 #+liber-documentation
 "@version{2025-01-01}
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
;;; pango_attr_iterator_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_copy" attr-iterator-copy)
    (g:boxed attr-iterator :return)
 #+liber-documentation
 "@version{2025-01-01}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @return{The newly allocated @class{pango:attr-iterator} instance.}
  @short{Copy a @class{pango:attr-iterator} instance.}
  @see-class{pango:attr-iterator}"
  (iterator (g:boxed attr-iterator)))

(export 'attr-iterator-copy)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_next
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_next" attr-iterator-next) :boolean
 #+liber-documentation
 "@version{2025-08-17}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @begin{return}
    @em{False} if the iterator is at the end of the attribute list, otherwise
    @em{true}.
  @end{return}
  @short{Advance the iterator until the next change of style.}
  @see-class{pango:attr-iterator}"
  (iterator (g:boxed attr-iterator)))

(export 'attr-iterator-next)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_range" %attr-iterator-range) :void
  (iterator (g:boxed attr-iterator))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun attr-iterator-range (iterator)
 #+liber-documentation
 "@version{2025-08-17}
  @syntax{(pango:attr-iterator-range iterator) => start, end}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @argument[start]{an integer for the start index of the range}
  @argument[end]{an integer for the end index of the range}
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
;;; pango_attr_iterator_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_get" attr-iterator-get)
    (g:boxed attribute)
 #+liber-documentation
 "@version{2025-08-17}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @argument[type]{a @sym{pango:attr-type} value for the type of attribute to
    find}
  @begin{return}
    The current @class{pango:attribute} instance of the given type, or
    @code{nil} if no attribute of that type applies to the current location.
  @end{return}
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
;;; pango_attr_iterator_get_font
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_get_font" %attr-iterator-font) :void
  (iterator (g:boxed attr-iterator))
  (desc (g:boxed font-description))
  (language :pointer)
  (attrs :pointer))

(defun attr-iterator-font (iterator)
 #+liber-documentation
 "@version{2025-08-24}
  @syntax{(pango:attr-iterator-font iterator) => desc, language, attrs}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @argument[desc]{a @class{pango:font-description} instance for the current
    values}
  @argument[language]{a @class{pango:language} instance for the language tag
    at the current position, or @code{nil} if none is found}
  @argument[attrs]{a list of @class{pango:attribute} instances with non-font
    attributes at the the current position, only the highest priority value of
    each attribute will be added to this list}
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
;;; pango_attr_iterator_get_attrs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_attr_iterator_get_attrs" attr-iterator-attrs)
    (g:slist-t (g:boxed attribute :return))
 #+liber-documentation
 "@version{2025-08-17}
  @argument[iterator]{a @class{pango:attr-iterator} instance}
  @begin{return}
    The list of all @class{pango:attribute} instances for the current range.
  @end{return}
  @begin{short}
    Gets a list of all attributes at the current position of the iterator.
  @end{short}
  @see-class{pango:attr-iterator}
  @see-class{pango:attribute}"
  (iterator (g:boxed attr-iterator)))

(export 'attr-iterator-attrs)

;;; ----------------------------------------------------------------------------
;;; pango_attr_iterator_destroy                             not needed
;;;
;;; Destroy a PangoAttrIterator and free all associated memory.
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.attributes.lisp --------------------------------------
