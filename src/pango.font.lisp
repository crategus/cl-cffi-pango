;;; ----------------------------------------------------------------------------
;;; pango.fonts.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.50 and modified to document the Lisp binding to the Pango library.
;;; See <http://www.pango.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; Fonts
;;;
;;;     Structures representing abstract fonts
;;;
;;; Types and Values
;;;
;;;     PangoFontDescription
;;;     PangoStyle
;;;     PangoWeight
;;;     PangoVariant
;;;     PangoStretch
;;;     PangoFontMask
;;;     PangoFontMetrics
;;;     PangoFont
;;;     PangoFontFamily
;;;     PangoFontFace
;;;     PangoFontMap
;;;     PangoFontMapClass
;;;     PangoFontset
;;;     PangoFontsetClass
;;;
;;; Functions
;;;
;;;     pango_font_description_new
;;;     pango_font_description_copy
;;;     pango_font_description_copy_static
;;;     pango_font_description_hash
;;;     pango_font_description_equal
;;;     pango_font_description_free
;;;     pango_font_descriptions_free
;;;     pango_font_description_set_family
;;;     pango_font_description_set_family_static
;;;     pango_font_description_get_family
;;;     pango_font_description_set_style
;;;     pango_font_description_get_style
;;;     pango_font_description_set_variant
;;;     pango_font_description_get_variant
;;;     pango_font_description_set_weight
;;;     pango_font_description_get_weight
;;;     pango_font_description_set_stretch
;;;     pango_font_description_get_stretch
;;;     pango_font_description_set_size
;;;     pango_font_description_get_size
;;;     pango_font_description_set_absolute_size
;;;     pango_font_description_get_size_is_absolute
;;;     pango_font_description_set_gravity
;;;     pango_font_description_get_gravity
;;;     pango_font_description_set_variations
;;;     pango_font_description_set_variations_static
;;;     pango_font_description_get_variations
;;;     pango_font_description_get_set_fields
;;;     pango_font_description_unset_fields
;;;     pango_font_description_merge
;;;     pango_font_description_merge_static
;;;     pango_font_description_better_match
;;;     pango_font_description_from_string
;;;     pango_font_description_to_string
;;;     pango_font_description_to_filename
;;;
;;;     pango_font_metrics_ref
;;;     pango_font_metrics_unref
;;;     pango_font_metrics_get_ascent
;;;     pango_font_metrics_get_descent
;;;     pango_font_metrics_get_height
;;;     pango_font_metrics_get_approximate_char_width
;;;     pango_font_metrics_get_approximate_digit_width
;;;     pango_font_metrics_get_underline_thickness
;;;     pango_font_metrics_get_underline_position
;;;     pango_font_metrics_get_strikethrough_thickness
;;;     pango_font_metrics_get_strikethrough_position
;;;
;;;     pango_font_find_shaper
;;;     pango_font_describe
;;;     pango_font_describe_with_absolute_size
;;;     pango_font_get_face
;;;     pango_font_get_coverage
;;;     pango_font_has_char
;;;     pango_font_get_glyph_extents
;;;     pango_font_get_metrics
;;;     pango_font_get_font_map
;;;     pango_font_get_features
;;;     pango_font_get_hb_font
;;;
;;;     pango_font_family_get_name
;;;     pango_font_family_is_monospace
;;;     pango_font_family_is_variable
;;;     pango_font_family_list_faces
;;;     pango_font_family_get_face
;;;
;;;     pango_font_face_get_face_name
;;;     pango_font_face_list_sizes
;;;     pango_font_face_describe
;;;     pango_font_face_is_synthesized
;;;     pango_font_face_get_family
;;;
;;;     pango_font_map_create_context
;;;     pango_font_map_load_font
;;;     pango_font_map_load_fontset
;;;     pango_font_map_list_families
;;;     pango_font_map_get_family
;;;     pango_font_map_get_serial
;;;     pango_font_map_changed
;;;     pango_font_map_get_shape_engine_type
;;;
;;;     pango_fontset_get_font
;;;     pango_fontset_get_metrics
;;;     PangoFontsetForeachFunc
;;;     pango_fontset_foreach
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ├── PangoFontDescription
;;;     ╰── PangoFontMetrics
;;;
;;;     GEnum
;;;     ├── PangoStretch
;;;     ├── PangoStyle
;;;     ├── PangoVariant
;;;     ╰── PangoWeight
;;;
;;;     GFlags
;;;     ╰── PangoFontMask
;;;
;;;     GObject
;;;     ├── PangoFont
;;;     │   ╰── PangoFcFont
;;;     ├── PangoFontFace
;;;     ├── PangoFontFamily
;;;     ├── PangoFontMap
;;;     │   ╰── PangoFcFontMap
;;;     ╰── PangoFontset
;;;
;;; Known Derived Interfaces
;;;
;;;     PangoFont is required by PangoCairoFont.
;;;     PangoFontMap is required by PangoCairoFontMap.
;;;
;;; Implemented Interfaces
;;;
;;;     PangoFontFamily implements GListModel.
;;;     PangoFontMap implements GListModel.
;;;
;;; Description
;;;
;;;     Pango supports a flexible architecture where a particular rendering
;;;     architecture can supply an implementation of fonts. The PangoFont
;;;     structure represents an abstract rendering-system-independent font.
;;;     Pango provides routines to list available fonts, and to load a font of
;;;     a given description.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoFontDescription
;;; ----------------------------------------------------------------------------

(define-g-boxed-opaque font-description "PangoFontDescription"
  :alloc (error "PangoFontDescription cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'font-description)
      "CStruct"
      (documentation 'font-description 'type)
 "@version{#2021-1-6}
  @begin{short}
    The @sym{pango:font-description} structure represents the description of an
    ideal font. These structures are used both to list what fonts are available
    on the system and also for specifying the characteristics of a font to load.
  @end{short}
  @begin{pre}
(define-g-boxed-opaque font-description \"PangoFontDescription\"
  :alloc (error \"PangoFontDescription cannot be created from the Lisp side.\"))
  @end{pre}
  @see-class{pango:context}
  @see-class{pango:layout}")

(export (boxed-related-symbols 'font-description))

;;; ----------------------------------------------------------------------------
;;; enum PangoStyle
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoStyle" style
  (:export t
   :type-initializer "pango_style_get_type")
  (:normal 0)
  (:oblique 1)
  (:italic 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'style)
      "GEnum"
      (liber:symbol-documentation 'style)
 "@version{#2021-1-6}
  @begin{short}
    An enumeration specifying the various slant styles possible for a font.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoStyle\" style
  (:export t
   :type-initializer \"pango_style_get_type\")
  (:normal 0)
  (:oblique 1)
  (:italic 2))
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{The font is upright.}
    @entry[:oblique]{The font is slanted, but in a roman style.}
    @entry[:italic]{The font is slanted in an italic style.}
  @end{table}
  @see-class{pango:font-description}")

;;; ----------------------------------------------------------------------------
;;; enum PangoWeight
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoWeight" weight
  (:export t
   :type-initializer "pango_weight_get_type")
  (:thin 100)
  (:ultralight 200)
  (:light 300)
  (:semilight 350)
  (:book 380)
  (:normal 400)
  (:medium 500)
  (:semibold 600)
  (:bold 700)
  (:ultrabold 800)
  (:heavy 900)
  (:ultraheavy 1000))

#+liber-documentation
(setf (liber:alias-for-symbol 'weight)
      "GEnum"
      (liber:symbol-documentation 'weight)
 "@version{#2021-2-4}
  @begin{short}
    An enumeration specifying the weight (boldness) of a font.
  @end{short}
  This is a numerical value ranging from 100 to 1000, but there are some
  predefined values.
  @begin{pre}
(define-g-enum \"PangoWeight\" weight
  (:export t
   :type-initializer \"pango_weight_get_type\")
  (:thin 100)
  (:ultralight 200)
  (:light 300)
  (:semilight 350)
  (:book 380)
  (:normal 400)
  (:medium 500)
  (:semibold 600)
  (:bold 700)
  (:ultrabold 800)
  (:heavy 900)
  (:ultraheavy 1000))
  @end{pre}
  @begin[code]{table}
    @entry[:thin]{The thin weight.}
    @entry[:ultralight]{The ultralight weight.}
    @entry[:light]{The light weight.}
    @entry[:semilight]{The semilight weight.}
    @entry[:book]{The book weight.}
    @entry[:normal]{The default weight.}
    @entry[:medium]{The normal weight.}
    @entry[:semibold]{The semibold weight.}
    @entry[:bold]{The bold weight.}
    @entry[:ultrabold]{The ultrabold weight.}
    @entry[:heavy]{The heavy weight.}
    @entry[:ultraheavy]{The ultraheavy weight.}
  @end{table}
  @see-class{pango:font-description}
  @see-function{pango:font-description-weight}")

;;; ----------------------------------------------------------------------------
;;; enum PangoVariant
;;; ----------------------------------------------------------------------------

(define-g-enum "PangoVariant" variant
  (:export t
   :type-initializer "pango_variant_get_type")
  (:normal 0)
  (:small-caps 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'variant)
      "GEnum"
      (liber:symbol-documentation 'variant)
 "@version{#2021-1-6}
  @begin{short}
    An enumeration specifying capitalization variant of the font.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoVariant\" variant
  (:export t
   :type-initializer \"pango_variant_get_type\")
  (:normal 0)
  (:small-caps 1))
  @end{pre}
  @begin[code]{table}
    @entry[:normal]{A normal font.}
    @entry[:small-caps]{A font with the lower case characters replaced by
      smaller variants of the capital characters.}
  @end{table}
  @see-class{pango:font-description}")

;;; ----------------------------------------------------------------------------
;;; enum PangoStretch
;;; ----------------------------------------------------------------------------

(glib-init:at-init ()
  (cffi:foreign-funcall "pango_stretch_get_type" :size))

(define-g-enum "PangoStretch" stretch
  (:export t
   :type-initializer "pango_stretch_get_type")
  (:ultra-condensed 0)
  (:extra-condensed 1)
  (:condensed 2)
  (:semi-condensed 3)
  (:normal 4)
  (:semi-expanded 5)
  (:expanded 6)
  (:extra-expanded 7)
  (:ultra-expanded 8))

#+liber-documentation
(setf (liber:alias-for-symbol 'stretch)
      "GEnum"
      (liber:symbol-documentation 'stretch)
 "@version{#2021-1-6}
  @begin{short}
    An enumeration specifying the width of the font relative to other designs
    within a family.
  @end{short}
  @begin{pre}
(define-g-enum \"PangoStretch\" stretch
  (:export t
   :type-initializer \"pango_stretch_get_type\")
  (:ultra-condensed 0)
  (:extra-condensed 1)
  (:condensed 2)
  (:semi-condensed 3)
  (:normal 4)
  (:semi-expanded 5)
  (:expanded 6)
  (:extra-expanded 7)
  (:ultra-expanded 8))
  @end{pre}
  @begin[code]{table}
    @entry[:ultra-condensed]{Ultra condensed width.}
    @entry[:extra-condensed]{Extra condensed width.}
    @entry[:condensed]{Condensed width.}
    @entry[:semi-condensed]{Semi condensed width.}
    @entry[:normal]{The normal widt.}
    @entry[:semi-expanded]{Semi expanded width.}
    @entry[:expanded]{Expanded width.}
    @entry[:extra-expanded]{Extra expanded width.}
    @entry[:ultra-expanded]{Ultra expanded width.}
  @end{table}
  @see-class{pango:font-description}")

;;; ----------------------------------------------------------------------------
;;; enum PangoFontMask
;;; ----------------------------------------------------------------------------

(define-g-flags "PangoFontMask" font-mask
  (:export t
   :type-initializer "pango_font_mask_get_type")
  (:family #.(ash 1 0))
  (:style #.(ash 1 1))
  (:variant #.(ash 1 2))
  (:weight #.(ash 1 3))
  (:stretch #.(ash 1 4))
  (:size #.(ash 1 5))
  (:gravity #.(ash 1 6))
  #+pango-1-42
  (:variations #.(ash 1 7))
)

#+liber-documentation
(setf (liber:alias-for-symbol 'font-mask)
      "GFlags"
      (liber:symbol-documentation 'font-mask)
 "@version{#2021-1-6}
  @begin{short}
    The bits in a @sym{pango:font-mask} flags correspond to fields in a
    @class{pango:font-description} structure that have been set.
  @end{short}
  @begin{pre}
(define-g-flags \"PangoFontMask\" font-mask
  (:export t
   :type-initializer \"pango_font_mask_get_type\")
  (:family #.(ash 1 0))
  (:style #.(ash 1 1))
  (:variant #.(ash 1 2))
  (:weight #.(ash 1 3))
  (:stretch #.(ash 1 4))
  (:size #.(ash 1 5))
  (:gravity #.(ash 1 6))
  (:variations #.(ash 1 7)))
  @end{pre}
  @begin[code]{table}
    @entry[:family]{The font family is specified.}
    @entry[:style]{The font style is specified.}
    @entry[:variant]{The font variant is specified.}
    @entry[:weight]{The font weight is specified.}
    @entry[:stretch]{The font stretch is specified.}
    @entry[:size]{The font size is specified.}
    @entry[:gravity]{The font gravity is specified.}
    @entry[:variations]{OpenType font variations are specified. Since 1.42}
  @end{table}
  @see-class{pango:font-description}")

;;; ----------------------------------------------------------------------------
;;; struct PangoFontMetrics
;;; ----------------------------------------------------------------------------

(glib-init:at-init ()
  (cffi:foreign-funcall "pango_font_metrics_get_type" :size))

(define-g-boxed-opaque font-metrics "PangoFontMetrics"
  :alloc (error "PangoFontMetrics cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'font-metrics)
      "CStruct"
      (documentation 'font-metrics 'type)
 "@version{#2021-1-14}
  @begin{short}
    A @sym{pango:font-metrics} structure holds the overall metric information
    for a font, possibly restricted to a script.
  @end{short}
  The fields of this structure are private to implementations of a font backend.
  See the documentation of the corresponding getters for documentation of their
  meaning.
  @begin{pre}
(define-g-boxed-opaque font-metrics \"PangoFontMetrics\"
  :alloc (error \"PangoFontMetrics cannot be created from the Lisp side.\"))
  @end{pre}
  @see-class{pango:context}
  @see-function{pango:font-metrics-ascent}
  @see-function{pango:font-metrics-descent}
  @see-function{pango:font-metrics-height}
  @see-function{pango:font-metrics-approximate-char-width}
  @see-function{pango:font-metrics-approximate-digit-width}
  @see-function{pango:font-metrics-underline-thickness}
  @see-function{pango:font-metrics-underline-position}
  @see-function{pango:font-metrics-strikethrough-thickness}
  @see-function{pango:font-metrics-strikethrough-position}")

(export 'font-metrics)

;;; ----------------------------------------------------------------------------
;;; PangoFont
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoFont" font
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_font_get_type")
  nil)

#+liber-documentation
(setf (documentation 'font 'type)
 "@version{#2021-1-14}
  @begin{short}
    The @sym{pango:font} class is used to represent a font in a
    rendering-system-independent matter.
  @end{short}

  The @sym{pango:font} class contains one member which the implementation
  fills in.
  @begin[Warning]{dictionary}
    The @sym{pango:font} class is deprecated and should not be used in
    newly written code.
  @end{dictionary}
  @see-class{pango:font-map}")

;;; ----------------------------------------------------------------------------
;;; struct PangoFontFamily
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoFontFamily" font-family
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "pango_font_family_get_type")
  nil)

#+liber-documentation
(setf (documentation 'font-family 'type)
 "@version{#2021-1-6}
  @begin{short}
    The @sym{pango:font-family} class is used to represent a family of
    related font faces.
  @end{short}
  The faces in a family share a common design, but differ in slant, weight,
  width and other aspects.
  @begin[Warning]{dictionary}
    The @sym{pango:font-family} class is deprecated and should not be used
    in newly written code.
  @end{dictionary}
  @see-class{pango:font}
  @see-class{pango:font-face}")

;;; ----------------------------------------------------------------------------
;;; struct PangoFontFace
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoFontFace" font-face
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "pango_font_face_get_type")
  nil)

#+liber-documentation
(setf (documentation 'font-face 'type)
 "@version{#2021-1-6}
  @begin{short}
    The @sym{pango:font-face} class is used to represent a group of fonts
    with the same family, slant, weight, width, but varying sizes.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{pango:font-face} class is deprecated and should not be used in
    newly written code.
  @end{dictionary}
  @see-class{pango:font}
  @see-class{pango:font-family}")

;;; ----------------------------------------------------------------------------
;;; PangoFontMap
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoFontMap" font-map
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_font_map_get_type")
  nil)

#+liber-documentation
(setf (documentation 'font-map 'type)
 "@version{#2021-1-14}
  @begin{short}
    The @sym{pango:font-map} class represents the set of fonts available for a
    particular rendering system.
  @end{short}
  This is a virtual object with implementations being specific to particular
  rendering systems.

  The @sym{pango:font-map} class contains one member which the implementation
  fills in.
  @see-class{pango:fc-font-map}")

;;; ----------------------------------------------------------------------------
;;; struct PangoFontMapClass
;;;
;;; struct PangoFontMapClass {
;;;   GObjectClass parent_class;
;;;
;;;
;;;   PangoFont *   (*load_font)     (PangoFontMap               *fontmap,
;;;                                   PangoContext               *context,
;;;                                   const PangoFontDescription *desc);
;;;   void          (*list_families) (PangoFontMap               *fontmap,
;;;                                   PangoFontFamily          ***families,
;;;                                   int                        *n_families);
;;;   PangoFontset *(*load_fontset)  (PangoFontMap               *fontmap,
;;;                                   PangoContext               *context,
;;;                                   const PangoFontDescription *desc,
;;;                                   PangoLanguage              *language);
;;;
;;;   const char *shape_engine_type;
;;; };
;;;
;;; The PangoFontMapClass structure holds the virtual functions for a
;;; particular PangoFontMap implementation.
;;;
;;; GObjectClass parent_class;
;;;     parent GObjectClass.
;;;
;;; load_font ()
;;;     a function to load a font with a given description.
;;;     See pango_font_map_load_font().
;;;
;;; list_families ()
;;;     A function to list available font families.
;;;     See pango_font_map_list_families().
;;;
;;; load_fontset ()
;;;     a function to load a fontset with a given given description suitable
;;;     for a particular language. See pango_font_map_load_fontset().
;;;
;;; const char *shape_engine_type;
;;;     the type of rendering-system-dependent engines that can handle fonts
;;;     of this fonts loaded with this fontmap.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoFontset
;;; ----------------------------------------------------------------------------

(define-g-object-class "PangoFontset" fontset
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_fontset_get_type")
  nil)

#+liber-documentation
(setf (documentation 'fontset 'type)
 "@version{#2021-1-6}
  @begin{short}
    A @sym{pango:fontset} object represents a set of @class{pango:font} objects
    to use when rendering text.
  @end{short}
  It is the result of resolving a @class{pango:font-description} instance
  against a particular @class{pango:context} object. It has operations for
  finding the component font for a particular Unicode character, and for
  finding a composite set of metrics for the entire fontset.
  @see-class{pango:font}
  @see-class{pango:context}
  @see-class{pango:font-description}")

;;; ----------------------------------------------------------------------------
;;; struct PangoFontsetClass
;;;
;;; struct PangoFontsetClass {
;;;   GObjectClass parent_class;
;;;
;;;
;;;   PangoFont *       (*get_font)     (PangoFontset     *fontset,
;;;                      guint             wc);
;;;
;;;   PangoFontMetrics *(*get_metrics)  (PangoFontset     *fontset);
;;;   PangoLanguage *   (*get_language) (PangoFontset     *fontset);
;;;   void              (*foreach)      (PangoFontset           *fontset,
;;;                      PangoFontsetForeachFunc func,
;;;                      gpointer                data);
;;; };
;;;
;;; The PangoFontsetClass structure holds the virtual functions for a particular
;;; PangoFontset implementation.
;;;
;;; GObjectClass parent_class;
;;;     parent GObjectClass.
;;;
;;; get_font ()
;;;     a function to get the font in the fontset that contains the best glyph
;;;     for the given Unicode character; see pango_fontset_get_font().
;;;
;;; get_metrics ()
;;;     a function to get overall metric information for the fonts in the
;;;     fontset; see pango_fontset_get_metrics().
;;;
;;; get_language ()
;;;     a function to get the language of the fontset.
;;;
;;; foreach ()
;;;     a function to loop over the fonts in the fontset.
;;;     See pango_fontset_foreach().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_description_new ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_new" font-description-new)
    (g:boxed font-description)
 #+liber-documentation
 "@version{#2021-1-6}
  @begin{return}
    The newly allocated @class{pango:font-description} instance.
  @end{return}
  @begin{short}
    Creates a new font description instance with all fields unset.
  @end{short}
  @see-class{pango:font-description}")

(export 'font-description-new)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_copy ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_copy" font-description-copy)
    (g:boxed font-description)
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[desc]{a @class{pango:font-description} instance, may be @code{nil}}
  @begin{return}
    The newly allocated @class{pango:font-description} instance.
  @end{return}
  @begin{short}
    Makes a copy of a font description.
  @end{short}
  @see-class{pango:font-description}"
  (desc (g:boxed font-description)))

(export 'font-description-copy)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_copy_static ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("pango_font_description_copy_static" font-description-copy-static)
    (g:boxed font-description)
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance, may be @code{nil}}
  @begin{return}
    The newly allocated @class{pango:font-description} instance.
  @end{return}
  @begin{short}
    Like the function @fun{pango:font-description-copy}, but only a shallow
    copy is made of the family name and other allocated fields.
  @end{short}
  The result can only be used until @arg{desc} is modified or freed. This is
  meant to be used when the copy is only needed temporarily.
  @see-class{pango:font-description}
  @see-function{pango:font-description-copy}"
  (desc (g:boxed font-description)))

;;; ----------------------------------------------------------------------------
;;; pango_font_description_hash ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_hash" font-description-hash) :uint
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[desc]{a @class{pango:font-description} instance}
  @return{An unsigned integer with the hash value.}
  @begin{short}
    Computes a hash of a font description.
  @end{short}
  @see-class{pango:font-description}"
  (desc (g:boxed font-description)))

(export 'font-description-hash)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_equal ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_equal" font-description-equal) :boolean
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc1]{a @class{pango:font-description} instance}
  @argument[desc2]{another @class{pango:font-description} instance}
  @begin{return}
    @em{True} if the two font descriptions are identical, @em{false} otherwise.
  @end{return}
  @begin{short}
    Compares two font descriptions for equality.
  @end{short}
  Two font descriptions are considered equal if the fonts they describe are
  provably identical. This means that their masks do not have to match, as long
  as other fields are all the same. Two font descriptions may result in
  identical fonts being loaded, but still compare @em{false}.
  @see-class{pango:font-description}"
  (desc1 (g:boxed font-description))
  (desc2 (g:boxed font-description)))

(export 'font-description-equal)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_free ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_free" font-description-free) :void
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[desc]{a @class{pango:font-description} instance}
  @short{Frees a font description.}
  @see-class{pango:font-description}"
  (desc (g:boxed font-description)))

(export 'font-description-free)

;;; ----------------------------------------------------------------------------
;;; pango_font_descriptions_free ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("pango_font_descriptions_free" font-descriptions-free) :void
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[descs]{a pointer to an array of @class{pango:font-description}
    instances}
  @argument[n-descs]{an integer with the number of font descriptions in
    @arg{descs}}
  @short{Frees an array of Pango font descriptions.}
  @see-class{pango:font-description}"
  (descs (:pointer (g:boxed font-description)))
  (n-descs :int))

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_family ()
;;; pango_font_description_set_family () -> font-description-family
;;; ----------------------------------------------------------------------------

(defun (setf font-description-family) (family desc)
  (cffi:foreign-funcall "pango_font_description_set_family"
                        (g:boxed font-description) desc
                        :string family
                        :void)
  family)

(defcfun ("pango_font_description_get_family" font-description-family)
    :string
 #+liber-documentation
 "@version{#2021-1-14}
  @syntax[]{(pango:font-description-family desc) => family}
  @syntax[]{(setf (pango:font-description-family desc) family)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[family]{a string representing the family name}
  @begin{short}
    Accessor of the family name for the font description.
  @end{short}

  The function @sym{pango:font-description-family} gets the family name field of
  a font description. The function @sym{(setf pango:font-description-family)}
  sets the family name field of a font description. The family name represents
  a family of related font styles, and will resolve to a particular
  @class{pango:font-family} object. In some uses of a
  @class{pango:font-description} structure, it is also possible to use a comma
  separated list of family names for this field.
  @see-class{pango:font-description}
  @see-class{pango:font-family}"
  (desc (g:boxed font-description)))

(export 'font-description-family)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_family_static ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("pango_font_description_set_family_static"
           font-description-set-family-static) :void
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[family]{a string representing the family name}
  @begin{short}
    Like the function @fun{pango:font-description-family}, except that no copy
    of @arg{family} is made.
  @end{short}
  The caller must make sure that the string passed in stays around until
  @arg{desc} has been freed or the name is set again. This function can be used
  if @arg{family} is a static string such as a C string literal, or if desc is
  only needed temporarily.
  @see-class{pango:font-description}
  @see-function{pango:font-description-family}"
  (desc (g:boxed font-description))
  (family :string))

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_style ()
;;; pango_font_description_set_style () -> font-description-style
;;; ----------------------------------------------------------------------------

(defun (setf font-description-style) (style desc)
  (cffi:foreign-funcall "pango_font_description_set_style"
                        (g:boxed font-description) desc
                        style style
                        :void)
  style)

(defcfun ("pango_font_description_get_style" font-description-style) style
 #+liber-documentation
 "@version{#2021-1-14}
  @syntax[]{(pango:font-description-style desc) => style}
  @syntax[]{(setf (pango:font-description-style desc) style)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[style]{the style of type @symbol{pango:style} for the font
    description}
  @begin{short}
    Accessor of the style field for the font description.
  @end{short}

  The function @sym{pango:font-description-style} gets the style field of a
  font description instance. The function @sym{(pango:font-description-style)}
  sets the style field. The @symbol{pango:style} enumeration describes whether
  the font is slanted and the manner in which it is slanted. It can be either
  @code{:normal}, @code{:italic}, or @code{:oblique}. Most fonts will either
  have a italic style or an oblique style, but not both, and font matching in
  Pango will match italic specifications with oblique fonts and vice-versa if
  an exact match is not found.
  @see-class{pango:font-description}
  @see-symbol{pango:style}"
  (desc (g:boxed font-description)))

(export 'font-description-style)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_variant ()
;;; pango_font_description_set_variant () -> font-description-variant
;;; ----------------------------------------------------------------------------

(defun (setf font-description-variant) (variant desc)
  (cffi:foreign-funcall "pango_font_description_set_variant"
                        (g:boxed font-description) desc
                        variant variant
                        :void)
  variant)

(defcfun ("pango_font_description_get_variant" font-description-variant) variant
 #+liber-documentation
 "@version{#2021-1-14}
  @syntax[]{(pango:font-description-variant desc) => variant}
  @syntax[]{(setf (pango:font-description-variant desc) variant)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[variant]{a @symbol{pango:variant} value with the variant type for
    the font description}
  @begin{short}
    Accessor of the variant field for the font description.
  @end{short}

  The function @sym{pango:font-description-variant} gets the variant field of a
  font description. The function @sym{(setf pango:font-descripton-variant)}
  sets the variant field. The @symbol{pango:variant} value can either be
  @code{:normal} or @code{:small-caps}.
  @see-class{pango:font-description}
  @see-symbol{pango:variant}"
  (desc (g:boxed font-description)))

(export 'font-description-variant)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_weight ()
;;; pango_font_description_set_weight () -> font-description-weight
;;; ----------------------------------------------------------------------------

;; FIXME: The pango:weight enumeration contains predefined values for the weight
;; of a font, but the weight can be any postive integer, therefore  we declare
;; the type as an integer and not pango:weight, generalize this behavior

(defun (setf font-description-weight) (weight desc)
  (cffi:foreign-funcall "pango_font_description_set_weight"
                        (g:boxed font-description) desc
                        :int weight ; not pango:weight
                        :void)
  weight)

(defcfun ("pango_font_description_get_weight" font-description-weight)
    :int ; not pango:weight
 #+liber-documentation
 "@version{#2021-2-4}
  @syntax[]{(pango:font-description-weight desc) => weight}
  @syntax[]{(setf (pango:font-description-weight desc) weight)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[weight]{an integer for the weight of the font description}
  @begin{short}
    Accessor of the weight field of a font description.
  @end{short}

  The function @sym{pango:font-description-weight} gets the weight field of a
  font description. The function @sym{(setf pango:font-description-weight)}
  sets the weight field. The weight field specifies how bold or light the font
  should be. In addition to the values of the @symbol{pango:weight} enumeration,
  other intermediate numeric values are possible.
  @see-class{pango:font-description}
  @see-symbol{pango:weight}"
  (desc (g:boxed font-description)))

(export 'font-description-weight)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_stretch ()
;;; pango_font_description_set_stretch () -> font-description-stretch
;;; ----------------------------------------------------------------------------

(defun (setf font-description-stretch) (stretch desc)
  (cffi:foreign-funcall "pango_font_description_set_stretch"
                        (g:boxed font-description) desc
                        stretch stretch
                        :void)
  stretch)

(defcfun ("pango_font_description_get_stretch" font-description-stretch) stretch
 #+liber-documentation
 "@version{#2021-1-14}
  @syntax[]{(pango:font-description-stretch desc) => stretch}
  @syntax[]{(setf (pango:font-description-stretch desc) stretch)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[stretch]{a @symbol{pango:stretch} value for the stretch for the
    font description}
  @begin{short}
    Accessor of the stretch field of the font description.
  @end{short}

  The function @sym{pango:font-description} gets the stretch field of a font
  description. The function @sym{(setf pango:font-description-stretch)} sets
  the stretch field. The stretch field specifies how narrow or wide the font
  should be.
  @see-class{pango:font-description}
  @see-symbol{pango:stretch}"
  (desc (g:boxed font-description)))

(export 'font-description-stretch)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_size ()
;;; pango_font_description_set_size () -> font-description-size
;;; ----------------------------------------------------------------------------

(defun (setf font-description-size) (size desc)
  (cffi:foreign-funcall "pango_font_description_set_size"
                        (g:boxed font-description) desc
                        :int size
                        :void)
  size)

(defcfun ("pango_font_description_get_size" font-description-size) :int
 #+liber-documentation
 "@version{#2021-1-14}
  @syntax[]{(pango:font-description-size desc) => size}
  @syntax[]{(setf (pango:font-description-size desc) size)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[size]{an integer with the size of the font in points, scaled by
    @var{+pango-scale+}}
  @begin{short}
    Accessor of the size field of a font description.
  @end{short}

  The function @sym{pango:font-description-size} gets the size field of a font
  description in points or device units. The function
  @sym{(setf pango:font-description-size)} sets the size field. This is mutually
  exclusive with the function @fun{pango:font-description-set-absolute-size}.

  A size value of 10 * @code{+pango-scale+} is a 10 point font. The conversion
  factor between points and device units depends on the system configuration
  and the output device. For screen display, a logical DPI of 96 is common, in
  which case a 10 point font corresponds to a 10 * (96 / 72) = 13.3 pixel font.
  Use the function @fun{pango:font-description-set-absolute-size} if you need a
  particular size in device units.

  You must call the function @fun{pango:font-description-size-is-absolute}
  to find out which is the case. Returns 0 if the size field has not previously
  been set or it has been set to 0 explicitly. Use the function
  @fun{pango:font-description-set-fields} to find out if the field was
  explicitly set or not.
  @see-class{pango:font-description}
  @see-function{pango:font-description-set-absolute-size}
  @see-function{pango:font-description-size-is-absolute}
  @see-function{pango:font-description-set-fields}"
  (desc (g:boxed font-description)))

(export 'font-description-size)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_absolute_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_set_absolute_size"
          %font-description-set-absolute-size) :void
  (desc (g:boxed font-description))
  (size :double))

(defun font-description-set-absolute-size (desc size)
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[size]{a double float with the new size, in Pango units}
  @begin{short}
    Sets the size field of a font description, in device units.
  @end{short}

  There are @var{+pango-scale+} Pango units in one device unit. For an output
  backend where a device unit is a pixel, a size value of 10 *
  @code{+pango-scale+} gives a 10 pixel font.

  This is mutually exclusive with the function @fun{pango:font-description-size}
  which sets the font size in points.
  @see-class{pango:font-description}
  @see-function{pango:font-description-size}"
  (%font-description-set-absolute-size desc (coerce size 'double-float)))

(export 'font-description-set-absolute-size)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_size_is_absolute ()
;;;   -> font-description-size-is-absolute
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_get_size_is_absolute"
           font-description-size-is-absolute) :boolean
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[desc]{a @class{pango:font-description} instance}
  @begin{return}
    Whether the size for the font description is in points or device units.
  @end{return}
  @begin{short}
    Determines whether the size of the font is in points (not absolute) or
    device units (absolute).
  @end{short}
  See the functions @fun{pango:font-description-size} and
  @fun{pango:font-description-set-absolute-size}.

  Use the function @fun{pango:font-description-set-fields} to find out if the
  size field of the font description was explicitly set or not.
  @see-class{pango:font-description}
  @see-function{pango:font-description-size}
  @see-function{pango:font-description-set-absolute-size}
  @see-function{pango:font-description-set-fields}"
  (desc (g:boxed font-description)))

(export 'font-description-size-is-absolute)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_gravity ()
;;; pango_font_description_set_gravity () -> font-description-gravity
;;; ----------------------------------------------------------------------------

(defun (setf font-description-gravity) (gravity desc)
  (cffi:foreign-funcall "pango_font_description_set_gravity"
                        (g:boxed font-description) desc
                        gravity gravity
                        :void)
  gravity)

(defcfun ("pango_font_description_get_gravity" font-description-gravity)
    gravity
 #+liber-documentation
 "@version{#2021-1-14}
  @syntax[]{(pango:font-description-gravity desc) => gravity}
  @syntax[]{(setf (pango:font-description-gravity desc) gravity)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[gravity]{a @symbol{pango:gravity} value for the gravity for the font
    description}
  @begin{short}
    Accessor of the gravity field of the font description.
  @end{short}

  The function @sym{pango:font-description-gravity} gets the gravity field of a
  font description. The function @sym{(setf pango:font-description-gravity)}
  sets the gravity field. The gravity field specifies how the glyphs should be
  rotated. If gravity is @code{:auto}, this actually unsets the gravity mask on
  the font description.

  This function is seldom useful to the user. Gravity should normally be set
  on a @class{pango:context} object.
  @see-class{pango:font-description}
  @see-class{pango:context}
  @see-symbol{pango:gravity}"
  (desc (g:boxed font-description)))

(export 'font-description-gravity)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_variations ()
;;; pango_font_description_set_variations ()
;;;   -> font-descriptions-variations
;;; ----------------------------------------------------------------------------

#+pango-1-42
(defun (setf font-description-variations) (variations desc)
  (cffi:foreign-funcall "pango_font_description_set_variations"
                        (g:boxed font-description) desc
                        :string variations
                        :void)
  variations)

#+pango-1-42
(defcfun ("pango_font_description_get_variations"
           font-description-variations) :string
 #+liber-documentation
 "@version{#2021-1-7}
  @syntax[]{(pango:font-description-variations desc) => variations}
  @syntax[]{(setf (pango:font-description-variations desc) variations)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[variations]{a string representing the variations}
  @begin{short}
    Accessor of the variations field for the font description.
  @end{short}

  The function @sym{pango:font-description-variations} gets the variations
  field of a font description. The function
  @sym{(setf pango:font-description-variations)} sets the variations field.
  OpenType font variations allow to select a font instance by specifying values
  for a number of axes, such as width or weight.

  The format of the variations string is @code{AXIS1=VALUE,AXIS2=VALUE...,}
  with each @code{AXIS} a 4 character tag that identifies a font axis, and each
  @code{VALUE} a floating point number. Unknown axes are ignored, and values
  are clamped to their allowed range.

  Pango does not currently have a way to find supported axes of a font. Both
  the HarfBuzz or FreeType libraries have API for this.

  Since 1.42
  @see-class{pango:font-description}"
  (desc (g:boxed font-description)))

#+pango-1-42
(export 'font-description-variations)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_variations_static ()
;;; ----------------------------------------------------------------------------

;; not exported

#+pango-1-42
(defcfun ("pango_font_description_set_variations_static"
           font-description-set-variations-static) :void
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[variations]{a string representing the variations}
  @begin{short}
    Like the function @fun{pango:font-description-variations}, except that no
    copy of variations is made.
  @end{short}
  The caller must make sure that the string passed in stays around until
  @arg{desc} has been freed or the name is set again. This function can be used
  if variations is a static string such as a C string literal, or if @arg{desc}
  is only needed temporarily.

  Since 1.42
  @see-class{pango:font-description}
  @see-function{pango:font-description-variations}"
  (desc (g:boxed font-description))
  (variations :string))

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_set_fields () -> font-description-set-fields
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_get_set_fields"
           font-description-set-fields) font-mask
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance}
  @begin{return}
    A list with @symbol{pango:font-mask} values set corresponding to the fields
    in @arg{desc} that have been set.
  @end{return}
  @begin{short}
    Determines which fields in a font description have been set.
  @end{short}
  @see-class{pango:font-description}
  @see-symbol{pango:font-mask}"
  (desc (g:boxed font-description)))

(export 'font-description-set-fields)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_unset_fields ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_unset_fields"
           font-description-unset-fields) :void
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[to-unset]{a list of @symbol{pango:font-mask} values in @arg{desc}
    to unset}
  @begin{short}
    Unsets some of the fields in a font description.
  @end{short}
  The unset fields will get back to their default values.
  @see-class{pango:font-description}
  @see-symbol{pango:font-mask}"
  (desc (g:boxed font-description))
  (to-unset font-mask))

(export 'font-description-unset-fields)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_merge ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_merge" font-description-merge) :void
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[desc-to-merge]{a @class{pango:font-description} instance to merge
    from, or @code{nil}}
  @argument[replace-existing]{if @em{true}, replace fields in @arg{desc} with
    the corresponding values from @arg{desc-to-merge}, even if they are already
    exist}
  @begin{short}
    Merges the fields that are set in @arg{desc-to-merge} into the fields in
    @arg{desc}.
  @end{short}
  If @arg{replace-existing} is @em{false}, only fields in @arg{desc} that are
  not already set are affected. If @em{true}, then fields that are already set
  will be replaced as well.

  If @arg{desc-to-merge} is @code{nil}, this function performs nothing.
  @see-class{pango:font-description}"
  (desc (g:boxed font-description))
  (desc-to-merge (g:boxed font-description))
  (replace-existing :boolean))

(export 'font-description-merge)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_merge_static ()
;;; ----------------------------------------------------------------------------

;; not exported

(defcfun ("pango_font_description_merge_static"
           font-description-merge-static) :void
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[desc-to-merge]{a @class{pango:font-description} instance to merge
    from}
  @argument[replace-existing]{if @em{true}, replace fields in @arg{desc} with
    the corresponding values from @arg{desc-to-merge}, even if they are already
    exist}
  @begin{short}
    Like the function @fun{pango:font-description-merge}, but only a shallow
    copy is made of the family name and other allocated fields.
  @end{short}
  @arg{desc} can only be used until @arg{desc_to_merge} is modified or freed.
  This is meant to be used when the merged font description is only needed
  temporarily.
  @see-class{pango:font-description}
  @see-function{pango:font-description-merge}"
  (desc (g:boxed font-description))
  (desc-to-merge (g:boxed font-description))
  (replace-existing :boolean))

;;; ----------------------------------------------------------------------------
;;; pango_font_description_better_match ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_better_match"
           font-description-better-match) :boolean
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[old-match]{a @class{pango:font-description} instance, or @code{nil}}
  @argument[new-match]{a @class{pango:font-description} instance}
  @return{@em{True} if @arg{new-match} is a better match.}
  @begin{short}
    Determines if the style attributes of @arg{new-match} are a closer match
    for @arg{desc} than those of @arg{old-match} are, or if @arg{old-match} is
    @code{nil}, determines if @arg{new-match} is a match at all.
  @end{short}
  Approximate matching is done for weight and style. Other style attributes
  must match exactly. Style attributes are all attributes other than family and
  size-related attributes. Approximate matching for style considers
  @code{:oblique} and @code{:italic} as matches, but not as good a match as
  when the styles are equal.

  Note that @arg{old-match} must match @arg{desc}.
  @see-class{pango:font-description}"
  (desc (g:boxed font-description))
  (old-match (g:boxed font-description))
  (new-match (g:boxed font-description)))

(export 'font-description-better-match)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_from_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_from_string"
           font-description-from-string)
    (g:boxed font-description)
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[str]{a string representation of a font description}
  @return{A new @class{pango:font-description} instance.}
  @begin{short}
    Creates a new font description from a string representation.
  @end{short}
  The string representation has the form \"@code{[FAMILY-LIST] [STYLE-OPTIONS]
  [SIZE] [VARIATIONS]}\", where @code{FAMILY-LIST} is a comma-separated list of
  families optionally terminated by a comma, @code{STYLE_OPTIONS} is a
  whitespace-separated list of words where each word describes one of style,
  variant, weight, stretch, or gravity, and @code{SIZE} is a decimal number
  (size in points) or optionally followed by the unit modifier \"px\" for
  absolute size. @code{VARIATIONS} is a comma-separated list of font variation
  specifications of the form \"@code{@@axis=value}\" (the = sign is optional).

  The following words are understood as styles:
  @begin{pre}
\"Normal\", \"Roman\", \"Oblique\", \"Italic\".
  @end{pre}
  The following words are understood as variants:
  @begin{pre}
   \"Small-Caps\".
  @end{pre}
  The following words are understood as weights:
  @begin{pre}
\"Thin\", \"Ultra-Light\", \"Extra-Light\", \"Light\", \"Semi-Light\",
\"Demi-Light\", \"Book\", \"Regular\", \"Medium\", \"Semi-Bold\",
\"Demi-Bold\", \"Bold\", \"Ultra-Bold\", \"Extra-Bold\", \"Heavy\",
\"Black\", \"Ultra-Black\", \"Extra-Black\".
  @end{pre}
  The following words are understood as stretch values:
  @begin{pre}
\"Ultra-Condensed\", \"Extra-Condensed\", \"Condensed\", \"Semi-Condensed\",
\"Semi-Expanded\", \"Expanded\", \"Extra-Expanded\", \"Ultra-Expanded\".
  @end{pre}
  The following words are understood as gravity values:
  @begin{pre}
\"Not-Rotated\", \"South\", \"Upside-Down\", \"North\", \"Rotated-Left\",
\"East\", \"Rotated-Right\", \"West\".
  @end{pre}
  Any one of the options may be absent. If @code{FAMILY-LIST} is absent, then
  the family_name field of the resulting font description will be initialized
  to NULL. If @code{STYLE-OPTIONS} is missing, then all style options will be
  set to the default values. If @code{SIZE} is missing, the size in the
  resulting font description will be set to 0.

  A typical example:
  @begin{pre}
\"Cantarell Italic Light 15 @@wght=200\"
  @end{pre}
  @see-class{pango:font-description}
  @see-function{pango:font-description-to-string}"
  (str :string))

(export 'font-description-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_to_string ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_to_string"
           font-description-to-string) :string
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[desc]{a @class{pango:font-description} instance}
  @return{A new string with a representation of a font description.}
  @begin{short}
    Creates a string representation of a font description.
  @end{short}
  See the function @fun{pango:font-description-from-string} for a description
  of the format of the string representation. The family list in the string
  description will only have a terminating comma if the last word of the list
  is a valid style option.
  @see-class{pango:font-description}
  @see-function{pango:font-description-from-string}"
  (desc (g:boxed font-description)))

(export 'font-description-to-string)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_to_filename ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_description_to_filename"
           font-description-to-filename) :string
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[desc]{a @class{pango:font-description} instance}
  @return{A new string with a filname.}
  @begin{short}
    Creates a filename representation of a font description.
  @end{short}
  The filename is identical to the result from calling the function
  @fun{pango:font-description-to-string}, but with underscores instead of
  characters that are untypical in filenames, and in lower case only.
  @see-class{pango:font-description}
  @see-function{pango:font-description-to-string}"
  (desc (g:boxed font-description)))

(export 'font-description-to-filename)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_ref ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_ref" font-metrics-ref)
    (g:boxed font-metrics)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{a @class{pango:font-metrics} instance}
  @begin{short}
    Increase the reference count of a font metrics instance by one.
  @end{short}
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-ref)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_unref ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_unref" font-metrics-unref) :void
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @begin{short}
    Decrease the reference count of a font metrics instance by one.
  @end{short}
  If the result is zero, frees the instance and any associated memory.
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-unref)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_ascent () -> font-metrics-ascent
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_get_ascent" font-metrics-ascent) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{An integer with the ascent, in Pango units.}
  @begin{short}
    Gets the ascent from a font metrics structure.
  @end{short}
  The ascent is the distance from the baseline to the logical top of a line of
  text. The logical top may be above or below the top of the actual drawn ink.
  It is necessary to lay out the text to figure where the ink will be.
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-ascent)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_descent () -> font-metrics-descent
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_get_descent" font-metrics-descent) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{An integer with the descent, in Pango units.}
  @begin{short}
    Gets the descent from a font metrics instance.
  @end{short}
  The descent is the distance from the baseline to the logical bottom of a line
  of text. The logical bottom may be above or below the bottom of the actual
  drawn ink. It is necessary to lay out the text to figure where the ink will
  be.
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-descent)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_height () -> font-metrics-height
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defcfun ("pango_font_metrics_get_height" font-metrics-height) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{An integer with the height, in Pango units.}
  @begin{short}
    Gets the line height from a font metrics instance.
  @end{short}
  The line height is the distance between successive baselines in wrapped text.

  If the line height is not available, 0 is returned.

  Since 1.44
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

#+pango-1-44
(export 'font-metrics-height)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_approximate_char_width ()
;;;   -> font-metrics-approximate-char-width
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_get_approximate_char_width"
           font-metrics-approximate-char-width) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{An integer with the character width, in Pango units.}
  @begin{short}
    Gets the approximate character width for a font metrics instance.
  @end{short}
  This is merely a representative value useful, for example, for determining
  the initial size for a window. Actual characters in text will be wider and
  narrower than this.
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-approximate-char-width)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_approximate_digit_width ()
;;;   -> font-metrics-approximate-digit-width
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_get_approximate_digit_width"
           font-metrics-approximate-digit-width) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{An integer with the digit width, in Pango units.}
  @begin{short}
    Gets the approximate digit width for a font metrics instance.
  @end{short}
  This is merely a representative value useful, for example, for determining
  the initial size for a window. Actual digits in text can be wider or narrower
  than this, though this value is generally somewhat more accurate than the
  result of the function @fun{pango:font-metrics-approximate-char-width} for
  digits.
  @see-class{pango:font-metrics}
  @see-function{pango:font-metrics-approximate-char-width}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-approximate-digit-width)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_underline_thickness ()
;;;   -> font-metrics-underline-thickness
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_get_underline_thickness"
           font-metrics-underline-thickness) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{An integer with the suggested underline thickness, in Pango units.}
  @begin{short}
    Gets the suggested thickness to draw for the underline.
  @end{short}
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-underline-thickness)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_underline_position ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_get_underline_position"
           font-metrics-underline-position) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{An integer with the suggested underline position, in Pango units.}
  @begin{short}
    Gets the suggested position to draw the underline.
  @end{short}
  The value returned is the distance above the baseline of the top of the
  underline. Since most fonts have underline positions beneath the baseline,
  this value is typically negative.
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-underline-position)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_strikethrough_thickness ()
;;;   -> font-metrics-strikethrough-thickness
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_get_strikethrough_thickness"
           font-metrics-strikethrough-thickness) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @begin{return}
    An integer with the suggested strikethrough thickness, in Pango units.
  @end{return}
  @begin{short}
    Gets the suggested thickness to draw for the strikethrough.
  @end{short}
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-strikethrough-thickness)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_strikethrough_position ()
;;;   -> font-metrics-strikethrough-position
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_metrics_get_strikethrough_position"
           font-metrics-strikethrough-position) :int
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @begin{return}
    An integer with the suggested strikethrough position, in Pango units.
  @end{return}
  @begin{short}
    Gets the suggested position to draw the strikethrough.
  @end{short}
  The value returned is the distance above the baseline of the top of the
  strikethrough.
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-strikethrough-position)

;;; ----------------------------------------------------------------------------
;;; pango_font_find_shaper ()
;;;
;;; PangoEngineShape *
;;; pango_font_find_shaper (PangoFont *font,
;;;                         PangoLanguage *language,
;;;                         guint32 ch);
;;;
;;; Finds the best matching shaper for a font for a particular language tag and
;;; character point.
;;;
;;; Warning
;;;
;;; pango_font_find_shaper is deprecated and should not be used in
;;; newly written code. Shape engines are no longer used.
;;;
;;; font :
;;;     a PangoFont
;;;
;;; language :
;;;     the language tag
;;;
;;; ch :
;;;     a Unicode character.
;;;
;;; Returns :
;;;     the best matching shaper.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_describe ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_describe" font-describe)
    (g:boxed font-description)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[font]{a @class{pango:font} object}
  @return{A newly-allocated @class{pango:font-description} instance.}
  @begin{short}
    Returns a description of the font, with font size set in points.
  @end{short}
  Use the function @fun{pango:font-describe-with-absolute-size} if you want the
  font size in device units.
  @see-class{pango:font}
  @see-class{pango:font-description}
  @see-function{pango:font-describe-with-absolute-size}"
  (font (g:object font)))

(export 'font-describe)

;;; ----------------------------------------------------------------------------
;;; pango_font_describe_with_absolute_size ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_describe_with_absolute_size"
           font-describe-with-absolute-size)
    (g:boxed font-description)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[font]{a @class{pango:font} object}
  @return{A newly-allocated @class{pango:font-description} instance.}
  @begin{short}
    Returns a description of the font, with absolute font size set, in device
    units.
  @end{short}
  Use the function @fun{pango:font-describe} if you want the font size in
  points.
  @see-class{pango:font}
  @see-class{pango:font-description}"
  (font (g:object font)))

(export 'font-describe-with-absolute-size)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_face () -> font-face
;;; ----------------------------------------------------------------------------

#+pango-1-46
(defcfun ("pango_font_get_face" font-face) (g:object font-face)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[font]{a @class{pango:font} object}
  @return{The @class{pango:font-face} object.}
  @begin{short}
    Gets the Pango font face to which @arg{font} belongs.
  @end{short}

  Since 1.46
  @see-class{pango:font}
  @see-class{pango:font-face}"
  (font (g:object font)))

#+pango-1-46
(export 'font-face)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_coverage () -> font-coverage
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_get_coverage" font-coverage)
    (g:object coverage)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[font]{a @class{pango:font} object}
  @argument[language]{the @class{pango:language} tag}
  @return{A newly-allocated @class{pango:coverage} object.}
  @begin{short}
    Computes the coverage map for a given font and language tag.
  @end{short}
  @see-class{pango:font}
  @see-class{pango:language}
  @see-class{pango:coverage}"
  (font (g:object font))
  (language (g:boxed language)))

(export 'font-coverage)

;;; ----------------------------------------------------------------------------
;;; pango_font_has_char ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defcfun ("pango_font_has_char" font-has-char) :boolean
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[font]{a @class{pango:font} object}
  @argument[wc]{an unsigned integer with a Unicode character}
  @return{Returns whether the font provides a glyph for this character.}
  @begin{short}
    Returns @em{true} if @arg{font} can render @arg{wc}.
  @end{short}

  Since 1.44
  @see-class{pango:font}"
  (font (g:object font))
  (wc :int32)) ; for gunichar (see Glib Unicode Manipulation)

#+pango-1-44
(export 'font-has-char)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_glyph_extents ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_get_glyph_extents" font-glyph-extents) :void
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[font]{a @class{pango:font} object}
  @argument[glyph]{the glyph index of type @symbol{pango:glyph}}
  @argument[ink-rect]{a @symbol{pango:rectangle} instance used to store the
    extents of the glyph as drawn or NULL to indicate that the result is not
    needed}
  @argument[logical-rect]{a @symbol{pango:rectangle} instance used to store the
    logical extents of the glyph or NULL to indicate that the result is not
    needed}
  @begin{short}
    Gets the logical and ink extents of a glyph within a font.
  @end{short}
  The coordinate system for each rectangle has its origin at the base line and
  horizontal origin of the character with increasing coordinates extending to
  the right and down. The macros @code{PANGO_ASCENT()}, @code{PANGO_DESCENT()},
  @code{PANGO_LBEARING()}, and @code{PANGO_RBEARING()} can be used to convert
  from the extents rectangle to more traditional font metrics. The units of the
  rectangles are in 1/@code{+pango-scale+} of a device unit.

  If @arg{font} is @code{nil}, this function gracefully sets some sane values
  in the output variables and returns.
  @see-class{pango:font}
  @see-symbol{pango:glyph}"
  (font (g:object font))
  (glyph glyph)
  (ink-rect (:pointer (:struct rectangle)))
  (logical-rect (:pointer (:struct rectangle))))

(export 'font-glyph-extents)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_metrics () -> font-metrics
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_get_metrics" font-metrics)
    (g:boxed font-metrics)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[font]{a @class{pango:font} object}
  @argument[language]{a @class{pango:language} tag used to determine which
    script to get the metrics for, or @code{nil} to indicate to get the metrics
    for the entire font}
  @begin{return}
    A @class{pango:font-metrics} instance. The caller must call the function
    @fun{pango:font-metrics-unref} when finished using the object.
  @end{return}
  @begin{short}
    Gets overall metric information for a font.
  @end{short}
  Since the metrics may be substantially different for different scripts, a
  language tag can be provided to indicate that the metrics should be retrieved
  that correspond to the script(s) used by that language.

  If @arg{font} is @code{nil}, this function gracefully sets some sane values
  in the output variables and returns.
  @see-class{pango:font}
  @see-class{pango:language}
  @see-class{pango:font-metrics}
  @see-function{pango:font-metrics-unref}"
  (font (g:object font))
  (language (g:boxed language)))

(export 'font-metrics)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_font_map () -> font-font-map
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_get_font_map" font-font-map)
    (g:object font-map)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[font]{a @class{pango:font} object, or @code{nil}}
  @begin{return}
    The @class{pango:font-map} object for the font, or @code{nil} if
    @arg{font} is @code{nil}
  @end{return}
  @begin{short}
    Gets the font map for which the font was created.
  @end{short}

  Note that the font maintains a weak reference to the font map, so if all
  references to font map are dropped, the font map will be finalized even if
  there are fonts created with the font map that are still alive. In that case
  this function will return @code{nil}. It is the responsibility of the user to
  ensure that the font map is kept alive. In most uses this is not an issue
  as a @class{pango:context} object holds a reference to the font map.
  @see-class{pango:font}
  @see-class{pango:font-map}
  @see-class{pango:context}"
  (font (g:object font)))

(export 'font-font-map)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_features ()
;;;
;;; void
;;; pango_font_get_features (PangoFont *font,
;;;                          hb_feature_t *features,
;;;                          guint len,
;;;                          guint *num_features);
;;;
;;; Obtain the OpenType features that are provided by the font. These are
;;; passed to the rendering system, together with features that have been
;;; explicitly set via attributes.
;;;
;;; Note that this does not include OpenType features which the rendering
;;; system enables by default.
;;;
;;; font :
;;;     a PangoFont
;;;
;;; features :
;;;     Array to features in.
;;;
;;; len :
;;;     the length of features
;;;
;;; num_features :
;;;     the number of used items in features .
;;;
;;; Since 1.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_get_hb_font ()
;;;
;;; hb_font_t *
;;; pango_font_get_hb_font (PangoFont *font);
;;;
;;; Get a hb_font_t object backing this font.
;;;
;;; Note that the objects returned by this function are cached and immutable.
;;; If you need to make changes to the hb_font_t, use hb_font_create_sub_font().
;;;
;;; font :
;;;     a PangoFont
;;;
;;; Returns :
;;;     The hb_font_t object backing the font, or NULL if the font does not
;;;     have one.
;;;
;;; Since 1.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_font_family_get_name () -> font-family-name
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_family_get_name" font-family-name) :string
 #+liber-documentation
 "@version{#2021-2-11}
  @argument[family]{a @class{pango:font-family} object}
  @begin{return}
    A string with the name of @arg{family}.
  @end{return}
  @begin{short}
    Gets the name of the font family.
  @end{short}
  The name is unique among all fonts for the font backend and can be used in a
  @class{pango:font-description} instance to specify that a face from this
  family is desired.
  @see-class{pango:font-family}
  @see-class{pango:font-description}"
  (family (g:object font-family)))

(export 'font-family-name)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_is_monospace ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_family_is_monospace" font-family-is-monospace)
    :boolean
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[family]{a @class{pango:font-family} object}
  @return{@em{True} if @arg{family} is monospace.}
  @begin{short}
    A monospace font is a font designed for text display where the characters
    form a regular grid.
  @end{short}
  For Western languages this would mean that the advance width of all characters
  are the same, but this categorization also includes Asian fonts which include
  double-width characters: characters that occupy two grid cells. The function
  @code{g_unichar_iswide()} returns a result that indicates whether a character
  is typically double-width in a monospace font.

  The best way to find out the grid-cell size is to call the function
  @fun{pango:font-metrics-approximate-digit-width}, since the results of the
  function @fun{pango:font-metrics-approximate-char-width} may be affected by
  double-width characters.
  @see-class{pango:font-family}
  @see-function{pango:font-metrics-approximate-digit-width}
  @see-function{pango:font-metrics-approximate-char-width}"
  (family (g:object font-family)))

(export 'font-family-is-monospace)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_is_variable ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defcfun ("pango_font_family_is_variable" font-family-is-variable)
    :boolean
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[family]{a @class{pango:font-family} object}
  @return{@em{True} if @arg{family} is variable.}
  @begin{short}
    A variable font is a font which has axes that can be modified to produce
    different faces.
  @end{short}

  Since 1.44
  @see-class{pango:font-family}"
  (family (g:object font-family)))

#+pango-1-44
(export 'font-family-is-variable)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_list_faces ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_family_list_faces" %font-family-list-faces) :void
  (family (g:object font-family))
  (faces :pointer)
  (n-faces (:pointer :int)))

(defun font-family-list-faces (family)
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[family]{a @class{pango:font-family} object}
  @begin{return}
    A list of @class{pango:font-face} objects, or @code{nil}.
  @end{return}
  @begin{short}
    Lists the different font faces that make up @arg{family}.
  @end{short}
  The faces in a family share a common design, but differ in slant, weight,
  width and other aspects.
  @see-class{pango:font-family}
  @see-class{pango:font-face}"
  (with-foreign-objects ((faces-ptr :pointer) (n-faces :int))
    (%font-family-list-faces family faces-ptr n-faces)
    (loop with faces-ar = (cffi:mem-ref faces-ptr :pointer)
          for i from 0 below (cffi:mem-ref n-faces :int)
          for face = (cffi:convert-from-foreign
                         (cffi:mem-aref faces-ar :pointer i)
                         'g:object)
          collect face
          finally (glib:free faces-ar))))

(export 'font-family-list-faces)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_get_face () -> font-family-face
;;; ----------------------------------------------------------------------------

#+pango-1-46
(defcfun ("pango_font_family_get_face" %font-family-face)
    (g:object font-face)
  (family (g:object font-family))
  (name :string))

(defun font-family-face (family name)
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[family]{a @class{pango:font-family} object}
  @argument[name]{a string with the name of a face. If @arg{name} is @code{nil},
    the family's default face, fontconfig calls it \"Regular\", will be
    returned.}
  @begin{return}
    The @class{pango:font-face} object, or @code{nil} if no face with the given
    name exists.
  @end{return}
  @begin{short}
    Gets the Pango font face of @arg{family} with the given @arg{name}.
  @end{short}

  Since 1.46
  @see-class{pango:font-family}
  @see-class{pango:font-face}"
  (%font-family-face family (if name name (cffi:null-pointer))))

#+pango-1-46
(export 'font-family-face)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_get_face_name () -> font-face-face-name
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_face_get_face_name" font-face-face-name) :string
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[face]{a @class{pango:font-face} object}
  @begin{return}
    The face name for @arg{face}.
  @end{return}
  @begin{short}
    Gets a name representing the style of this face among the different faces
    in the @class{pango:font-family} for the face.
  @end{short}
  This name is unique among all faces in the family and is suitable for
  displaying to users.
  @see-class{pango:font-face}
  @see-class{pango:font-family}"
  (face (g:object font-face)))

(export 'font-face-face-name)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_list_sizes ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_face_list_sizes" %font-face-list-sizes) :void
  (face (g:object font-face))
  (sizes (:pointer :int))
  (n-sizes (:pointer :int)))

(defun font-face-list-sizes (face)
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[face]{a @class{pango:font-face} object}
  @return{A list of integer.}
  @begin{short}
    List the available sizes for a font.
  @end{short}
  This is only applicable to bitmap fonts. For scalable fonts, returns
  @code{nil}. The sizes returned are in Pango units and are sorted in ascending
  order.
  @see-class{pango:font-face}"
  (with-foreign-objects ((sizes-ptr :pointer) (n-sizes :int))
    (%font-face-list-sizes face sizes-ptr n-sizes)
    (loop with sizes-ar = (cffi:mem-ref sizes-ptr :pointer)
          for i from 0 below (cffi:mem-ref n-sizes :int)
          for size = (cffi:mem-aref sizes-ar :int i)
          collect size
          finally (glib:free sizes-ar))))

(export 'font-face-list-sizes)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_describe ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_face_describe" font-face-describe)
    (g:boxed font-description)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[face]{a @class{pango:font-face} object}
  @begin{return}
    A newly-created @class{pango:font-description} instance holding the
    description of @arg{face}. Use the function
    @fun{pango:font-description-free} to free the result.
  @end{return}
  @begin{short}
    Returns the family, style, variant, weight and stretch of a
    @class{pango:font-face} object.
  @end{short}
  The size field of the resulting font description will be unset.
  @see-class{pango:font-face}
  @see-class{pango:font-description}
  @see-function{pango:font-description-free}"
  (face (g:object font-face)))

(export 'font-face-describe)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_is_synthesized ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_face_is_synthesized" font-face-is-synthesized) :boolean
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[face]{a @class{pango:font-face} object}
  @return{A boolean whether @arg{face} is synthesized.}
  @begin{short}
    Returns whether a @class{pango:font-face} object is synthesized by the
    underlying font rendering engine from another face, perhaps by shearing,
    emboldening, or lightening it.
  @end{short}
  @see-class{pango:font-face}"
  (face (g:object font-face)))

(export 'font-face-is-synthesized)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_get_family () -> font-face-family
;;; ----------------------------------------------------------------------------

#+pango-1-46
(defcfun ("pango_font_face_get_family" font-face-family)
    (g:object font-family)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[face]{a @class{pango:font-face} object}
  @return{The @class{pango:font-family} object.}
  @begin{short}
    Gets the Pango font family that @arg{face} belongs to.
  @end{short}

  Since 1.46
  @see-class{pango:font-face}
  @see-class{pango:font-family}"
  (face (g:object font-face)))

#+pango-1-46
(export 'font-face-family)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_create_context ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_map_create_context" font-map-create-context)
    (g:object context)
 #+liber-documentation
 "@version{#2023-1-7}
  @argument[fontmap]{a @class{pango:font-map} object}
  @begin{return}
    The newly allocated @class{pango:context} object.
  @end{return}
  @begin{short}
    Creates a Pango context connected to @arg{fontmap}.
  @end{short}
  This is equivalent to the @fun{pango:context-new} function followed by
  the @fun{pango:context-font-map} function.

  If you are using Pango as part of a higher-level system, that system may have
  it's own way of create a Pango context. For instance, the GTK toolkit has,
  among others, the @fun{gdk:pango-context-for-screen}, and
  @fun{gtk:widget-pango-context} functions. Use those instead.
  @see-class{pango:font-map}
  @see-class{pango:context}
  @see-function{pango:context-new}
  @see-function{pango:context-font-map}
  @see-function{gdk:pango-context-for-screen}
  @see-function{gtk:widget-pango-context}"
  (fontmap (g:object font-map)))

(export 'font-map-create-context)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_load_font ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_map_load_font" font-map-load-font) (g:object font)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[fontmap]{a @class{pango:font-map} object}
  @argument[context]{the @class{pango:context} object the font will be used
    with}
  @argument[desc]{a @class{pango:font-description} instance describing the font
    to load}
  @begin{return}
    The newly allocated @class{pango:font} loaded, or @code{nil} if no font
    matched.
  @end{return}
  @begin{short}
    Load the font in the fontmap that is the closest match for @arg{desc}.
  @end{short}
  @see-class{pango:font-map}
  @see-class{pango:context}
  @see-class{pango:font-description}"
  (fontmap (g:object font-map))
  (context (g:object context))
  (desc (g:boxed font-description)))

(export 'font-map-load-font)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_load_fontset ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_map_load_fontset" font-map-load-fontset)
    (g:object fontset)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[fontmap]{a @class{pango:font-map} object}
  @argument[context]{the @class{pango:context} object the font will be used
    with}
  @argument[desc]{a @class{pango:font-description} instance describing the font
    to load}
  @argument[language]{a @class{pango:language} instance the fonts will be used
    for}
  @begin{return}
    The newly allocated @class{pango:fontset} object loaded, or @code{nil} if
    no font matched.
  @end{return}
  @begin{short}
    Load a set of fonts in the fontmap that can be used to render a font
    matching @arg{desc}.
  @end{short}
  @see-class{pango:font-map}
  @see-class{pango:context}
  @see-class{pango:font-description}
  @see-class{pango:language}"
  (fontmap (g:object font-map))
  (context (g:object context))
  (desc (g:boxed font-description))
  (language (g:boxed language)))

(export 'font-map-load-fontset)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_list_families ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_map_list_families" %font-map-list-families) :void
  (fontmap (g:object font-map))
  (families :pointer)
  (n-families (:pointer :int)))

(defun font-map-list-families (fontmap)
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[fontmap]{a @class{pango:font-map} object}
  @begin{return}
    A list of @class{pango:font-family} objects, or @code{nil}.
  @end{return}
  @begin{short}
    List all families for a font map.
  @end{short}
  @see-class{pango:font-map}
  @see-class{pango:font-family}"
  (with-foreign-objects ((families-ptr :pointer) (n-families :int))
    (%font-map-list-families fontmap families-ptr n-families)
    (loop with families-ar = (cffi:mem-ref families-ptr :pointer)
          for i from 0 below (cffi:mem-ref n-families :int)
          for family = (cffi:convert-from-foreign
                           (cffi:mem-aref families-ar :pointer i)
                           'g:object)
          collect family
          finally (glib:free families-ar))))

(export 'font-map-list-families)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_get_family () -> font-map-family
;;; ----------------------------------------------------------------------------

#+pango-1-46
(defcfun ("pango_font_map_get_family" font-map-family)
    (g:object font-family)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[fontmap]{a @class{pango:font-map} object}
  @argument[name]{a string with a family name}
  @return{The @class{pango:font-family} object.}
  @begin{short}
    Gets a font family by name.
  @end{short}

  Since 1.46
  @see-class{pango:font-map}
  @see-class{panto-font-family}"
  (fontmap (g:object font-map))
  (name :string))

#+pango-1-46
(export 'font-map-family)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_get_serial () -> font-map-serial
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_map_get_serial" font-map-serial) :uint
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[fontmap]{a @class{pango:font-map} object}
  @return{An unsigned integer with the current serial number of @arg{fontmap}.}
  @begin{short}
    Returns the current serial number of @arg{fontmap}.
  @end{short}
  The serial number is initialized to a small number larger than zero when a
  new fontmap is created and is increased whenever the fontmap is changed. It
  may wrap, but will never have the value 0. Since it can wrap, never compare
  it with \"less than\", always use \"not equals\".

  The fontmap can only be changed using backend-specific API, like changing
  fontmap resolution.

  This can be used to automatically detect changes to a Pango font map, like in
  a @class{pango:context} object.
  @see-class{pango:font-map}
  @see-class{pango:context}"
  (fontmap (g:object font-map)))

(export 'font-map-serial)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_font_map_changed" font-map-changed) :void
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[fontmap]{a @class{pango:font-map} object}
  @begin{short}
    Forces a change in the context, which will cause any @class{pango:context}
    object using this fontmap to change.
  @end{short}

  This function is only useful when implementing a new backend for Pango,
  something applications will not do. Backends should call this function if
  they have attached extra data to the context and such data is changed.
  @see-class{pango:font-map}
  @see-class{pango:context}"
  (fontmap (g:object font-map)))

(export 'font-map-changed)

;;; ----------------------------------------------------------------------------
;;; pango_fontset_get_font () -> fontset-font
;;; ----------------------------------------------------------------------------

(defcfun ("pango_fontset_get_font" fontset-font) (g:object font)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[fontset]{a @class{pango:fontset} object}
  @argument[wc]{an unsigned integer with a Unicode character}
  @return{A @class{pango:font} object.}
  @begin{short}
    Returns the font in the fontset that contains the best glyph for the
    Unicode character @arg{wc}.
  @end{short}
  @see-class{pango:fontset}
  @see-class{pango:font}"
  (fontset (g:object fontset))
  (wx :uint))

(export 'fontset-font)

;;; ----------------------------------------------------------------------------
;;; pango_fontset_get_metrics () -> fontset-metrics
;;; ----------------------------------------------------------------------------

(defcfun ("pango_fontset_get_metrics" fontset-metrics)
    (g:boxed font-metrics)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[fontset]{a @class{pango:fontset} object}
  @begin{return}
    A @class{pango:font-metrics} instance. The caller must call the function
    @fun{pango:font-metrics-unref} when finished using the object.
  @end{return}
  @begin{short}
    Get overall metric information for the fonts in the fontset.
  @end{short}
  @see-class{pango:fontset}
  @see-class{pango:font-metrics}"
  (fontset (g:object fontset)))

(export 'fontset-metrics)

;;; ----------------------------------------------------------------------------
;;; PangoFontsetForeachFunc ()
;;;
;;; gboolean (*PangoFontsetForeachFunc) (PangoFontset *fontset,
;;;                                      PangoFont *font,
;;;                                      gpointer data);
;;;
;;; A callback function used by pango_fontset_foreach() when enumerating the
;;; fonts in a fontset.
;;;
;;; fontset :
;;;     a PangoFontset
;;;
;;; font :
;;;     a font from fontset
;;;
;;; data :
;;;     callback data
;;;
;;; Returns :
;;;     if TRUE, stop iteration and return immediately.
;;;
;;; Since 1.4
;;; ----------------------------------------------------------------------------

(cffi:defcallback fontset-foreach-cb :boolean
    ((fontset (g:object fontset))
     (font (g:object font))
     (data :pointer))
  (restart-case
    (funcall (glib:get-stable-pointer-value data) fontset font)
    (return () :report "Error in PangoFontsetForeach function." nil)))

;;; ----------------------------------------------------------------------------
;;; pango_fontset_foreach ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_fontset_foreach" %fontset-foreach) :void
  (fontset (g:object fontset))
  (func :pointer)
  (data :pointer))

(defun fontset-foreach (fontset func)
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[fontset]{a @class{pango:fontset} object}
  @argument[func]{callback function}
  @begin{short}
    Iterates through all the fonts in a fontset, calling @arg{func} for each
    one.
  @end{short}
  If @arg{func} returns @em{true}, that stops the iteration.
  @see-class{pango:fontset}"
  (with-stable-pointer (ptr func)
    (%fontset-foreach fontset
                      (cffi:callback fontset-foreach-cb)
                      ptr)))

(export 'fontset-foreach)

;;; --- End of file pango.fonts.lisp -------------------------------------------
