;;; ----------------------------------------------------------------------------
;;; pango.font-description.lisp
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
;;; Types and Values
;;;
;;;     PangoStyle
;;;     PangoWeight
;;;     PangoVariant
;;;     PangoStretch
;;;     PangoFontMask
;;;
;;;     PangoFontDescription
;;;
;;; Functions
;;;
;;;     pango_font_description_new
;;;     pango_font_description_copy
;;;     pango_font_description_copy_static                 not implemented
;;;     pango_font_description_hash
;;;     pango_font_description_equal
;;;     pango_font_description_free                        not needed
;;;     pango_font_descriptions_free                       not needed
;;;     pango_font_description_set_family
;;;     pango_font_description_set_family_static           not implemented
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
;;;     pango_font_description_set_variations_static       not implemented
;;;     pango_font_description_get_variations
;;;     pango_font_description_get_set_fields
;;;     pango_font_description_unset_fields
;;;     pango_font_description_merge
;;;     pango_font_description_merge_static                not implemented
;;;     pango_font_description_better_match
;;;     pango_font_description_from_string
;;;     pango_font_description_to_string
;;;     pango_font_description_to_filename
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── PangoFontDescription
;;;
;;;     GEnum
;;;     ├── PangoStretch
;;;     ├── PangoStyle
;;;     ├── PangoVariant
;;;     ╰── PangoWeight
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoStyle
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoStyle" style
  (:export t
   :type-initializer "pango_style_get_type")
  (:normal 0)
  (:oblique 1)
  (:italic 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'style)
      "GEnum"
      (liber:symbol-documentation 'style)
 "@version{2023-8-27}
  @begin{declaration}
(gobject:define-genum \"PangoStyle\" style
  (:export t
   :type-initializer \"pango_style_get_type\")
  (:normal 0)
  (:oblique 1)
  (:italic 2))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:normal]{The font is upright.}
      @entry[:oblique]{The font is slanted, but in a roman style.}
      @entry[:italic]{The font is slanted in an italic style.}
    @end{table}
  @end{values}
  @begin{short}
    An enumeration specifying the various slant styles possible for a font.
  @end{short}
  @see-class{pango:font-description}
  @see-function{pango:font-description-style}")

;;; ----------------------------------------------------------------------------
;;; PangoWeight
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoWeight" weight
  (:export t
   :allow-undeclared-values t
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
 "@version{2024-3-3}
  @begin{declaration}
(gobject:define-genum \"PangoWeight\" weight
  (:export t
   :allow-undeclared-values t
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
  @end{declaration}
  @begin{values}
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
  @end{values}
  @begin{short}
    An enumeration specifying the weight (boldness) of a font.
  @end{short}
  This is a numerical value ranging from 100 to 1000, but there are some
  predefined values.
  @begin[Note]{dictionary}
    The enumeration allows undeclared values. Therefore, it is possible to pass
    any integer value to functions that expect a @symbol{pango:weight} value.
  @end{dictionary}
  @see-class{pango:font-description}
  @see-function{pango:font-description-weight}")

;;; ----------------------------------------------------------------------------
;;; PangoVariant
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoVariant" variant
  (:export t
   :type-initializer "pango_variant_get_type")
  (:normal 0)
  (:small-caps 1)
  #+pango-1-50
  (:all-small-caps 2)
  #+pango-1-50
  (:petite-caps 3)
  #+pango-1-50
  (:all-petite-caps 4)
  #+pango-1-50
  (:unicase 5)
  #+pango-1-50
  (:title-caps 6))

#+liber-documentation
(setf (liber:alias-for-symbol 'variant)
      "GEnum"
      (liber:symbol-documentation 'variant)
 "@version{2024-3-3}
  @begin{declaration}
(gobject:define-genum \"PangoVariant\" variant
  (:export t
   :type-initializer \"pango_variant_get_type\")
  (:normal 0)
  (:small-caps 1)
  #+pango-1-50
  (:all-small-caps 2)
  #+pango-1-50
  (:petite-caps 3)
  #+pango-1-50
  (:all-petite-caps 4)
  #+pango-1-50
  (:unicase 5)
  #+pango-1-50
  (:title-caps 6))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:normal]{A normal font.}
      @entry[:small-caps]{A font with the lower case characters replaced by
        smaller variants of the capital characters.}
      @entry[:all-small-caps]{A font with all characters replaced by smaller
        variants of the capital characters. Since 1.50}
      @entry[:petite-caps]{A font with the lower case characters replaced by
        smaller variants of the capital characters. Petite Caps can be even
        smaller than Small Caps. Since 1.50}
      @entry[:all-petite-caps]{A font with all characters replaced by smaller
        variants of the capital characters. Petite Caps can be even smaller
        than Small Caps. Since 1.50}
      @entry[:unicase]{A font with the upper case characters replaced by
        smaller variants of the capital letters. Since 1.50}
      @entry[:title-caps]{A font with capital letters that are more suitable
        for all-uppercase title. Since 1.50}
    @end{table}
  @end{values}
  @begin{short}
    An enumeration specifying capitalization variant of the font.
  @end{short}
  @see-class{pango:font-description}
  @see-function{pango:font-description-variant}")

;;; ----------------------------------------------------------------------------
;;; PangoStretch
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoStretch" stretch
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
 "@version{2024-3-3}
  @begin{declaration}
(gobject:define-genum \"PangoStretch\" stretch
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
  @end{declaration}
  @begin{values}
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
  @end{values}
  @begin{short}
    An enumeration specifying the width of the font relative to other designs
    within a family.
  @end{short}
  @see-class{pango:font-description}
  @see-function{pango:font-description-stretch}")

;;; ----------------------------------------------------------------------------
;;; PangoFontMask
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "PangoFontMask" font-mask
  (:export t
   :type-initializer "pango_font_mask_get_type")
  (:family #.(ash 1 0))
  (:style #.(ash 1 1))
  (:variant #.(ash 1 2))
  (:weight #.(ash 1 3))
  (:stretch #.(ash 1 4))
  (:size #.(ash 1 5))
  (:gravity #.(ash 1 6))
  (:variations #.(ash 1 7)))

#+liber-documentation
(setf (liber:alias-for-symbol 'font-mask)
      "GFlags"
      (liber:symbol-documentation 'font-mask)
 "@version{2023-8-27}
  @begin{declaration}
(gobject:define-gflags \"PangoFontMask\" font-mask
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
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:family]{The font family is specified.}
      @entry[:style]{The font style is specified.}
      @entry[:variant]{The font variant is specified.}
      @entry[:weight]{The font weight is specified.}
      @entry[:stretch]{The font stretch is specified.}
      @entry[:size]{The font size is specified.}
      @entry[:gravity]{The font gravity is specified.}
      @entry[:variations]{OpenType font variations are specified.}
    @end{table}
  @end{values}
  @begin{short}
    The bits correspond to fields in a @class{pango:font-description} instance
    that have been set.
  @end{short}
  @see-class{pango:font-description}
  @see-function{pango:font-description-set-fields}
  @see-function{pango:font-description-unset-fields}")

;;; ----------------------------------------------------------------------------
;;; PangoFontDescription
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_new" %font-description-new) :pointer)

(glib:define-gboxed-opaque font-description "PangoFontDescription"
  :export t
  :type-initializer "pango_font_description_get_type"
  :alloc (%font-description-new))

#+liber-documentation
(setf (liber:alias-for-class 'font-description)
      "GBoxed"
      (documentation 'font-description 'type)
 "@version{2023-8-27}
  @begin{declaration}
(glib:define-gboxed-opaque font-description \"PangoFontDescription\"
  :export t
  :type-initializer \"pango_font_description_get_type\"
  :alloc (%font-description-new))
  @end{declaration}
  @begin{short}
    The @class{pango:font-description} structure represents the description of
    an ideal font.
  @end{short}
  It is opaque, and has no user visible fields. These structures are used both
  to list what fonts are available on the system and also for specifying the
  characteristics of a font to load.
  @see-constructor{pango:font-description-new}
  @see-constructor{pango:font-description-copy}
  @see-class{pango:context}
  @see-class{pango:layout}")

;;; ----------------------------------------------------------------------------
;;; pango_font_description_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_new" font-description-new)
    (g:boxed font-description :return)
 #+liber-documentation
 "@version{2023-8-27}
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

(cffi:defcfun ("pango_font_description_copy" font-description-copy)
    (g:boxed font-description :return)
 #+liber-documentation
 "@version{2023-8-27}
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
;;; pango_font_description_copy_static ()                  not implemented
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_font_description_copy_static"
                font-description-copy-static) (g:boxed font-description)
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance, may be @code{nil}}
  @begin{return}
    The newly allocated @class{pango:font-description} instance.
  @end{return}
  @begin{short}
    Like the @fun{pango:font-description-copy} function, but only a shallow
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

(cffi:defcfun ("pango_font_description_hash" font-description-hash) :uint
 #+liber-documentation
 "@version{2023-8-27}
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

(cffi:defcfun ("pango_font_description_equal" font-description-equal) :boolean
 #+liber-documentation
 "@version{2023-8-27}
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
;;; pango_font_description_free ()                         not needed
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_font_description_free" font-description-free) :void
 #+liber-documentation
 "@version{#2021-1-14}
  @argument[desc]{a @class{pango:font-description} instance}
  @short{Frees a font description.}
  @see-class{pango:font-description}"
  (desc (g:boxed font-description)))

;;; ----------------------------------------------------------------------------
;;; pango_font_descriptions_free ()                        not needed
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_font_descriptions_free" font-descriptions-free) :void
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
;;; pango_font_description_set_family ()
;;; ----------------------------------------------------------------------------

(defun (setf font-description-family) (family desc)
  (cffi:foreign-funcall "pango_font_description_set_family"
                        (g:boxed font-description) desc
                        :string family
                        :void)
  family)

(cffi:defcfun ("pango_font_description_get_family" font-description-family)
    :string
 #+liber-documentation
 "@version{2024-3-3}
  @syntax{(pango:font-description-family desc) => family}
  @syntax{(setf (pango:font-description-family desc) family)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[family]{a string representing the family name}
  @begin{short}
    The @fun{pango:font-description-family} function gets the family name field
    of a font description.
  @end{short}
  The @setf{pango:font-description-family} function sets the family name field.
  The family name represents a family of related font styles, and will resolve
  to a particular @class{pango:font-family} object. In some uses of a
  @class{pango:font-description} instance, it is also possible to use a comma
  separated list of family names for this field.
  @see-class{pango:font-description}
  @see-class{pango:font-family}"
  (desc (g:boxed font-description)))

(export 'font-description-family)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_family_static ()            not implemented
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_font_description_set_family_static"
                font-description-set-family-static) :void
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[family]{a string representing the family name}
  @begin{short}
    Like the @fun{pango:font-description-family} function, except that no copy
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
;;; pango_font_description_set_style ()
;;; ----------------------------------------------------------------------------

(defun (setf font-description-style) (style desc)
  (cffi:foreign-funcall "pango_font_description_set_style"
                        (g:boxed font-description) desc
                        style style
                        :void)
  style)

(cffi:defcfun ("pango_font_description_get_style" font-description-style) style
 #+liber-documentation
 "@version{2024-3-3}
  @syntax{(pango:font-description-style desc) => style}
  @syntax{(setf (pango:font-description-style desc) style)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[style]{a @symbol{pango:style} value with the style for the font
    description}
  @begin{short}
    The @fun{pango:font-description-style} function gets the style field of a
    font description instance.
  @end{short}
  The @setf{pango:font-description-style} function sets the style field. The
  @symbol{pango:style} enumeration describes whether the font is slanted and
  the manner in which it is slanted. It can be either @code{:normal},
  @code{:italic}, or @code{:oblique}. Most fonts will either have a italic style
  or an oblique style, but not both, and font matching in Pango will match
  italic specifications with oblique fonts and vice-versa if an exact match is
  not found.
  @see-class{pango:font-description}
  @see-symbol{pango:style}"
  (desc (g:boxed font-description)))

(export 'font-description-style)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_variant ()
;;; pango_font_description_set_variant ()
;;; ----------------------------------------------------------------------------

(defun (setf font-description-variant) (variant desc)
  (cffi:foreign-funcall "pango_font_description_set_variant"
                        (g:boxed font-description) desc
                        variant variant
                        :void)
  variant)

(cffi:defcfun ("pango_font_description_get_variant" font-description-variant)
    variant
 #+liber-documentation
 "@version{2024-3-3}
  @syntax{(pango:font-description-variant desc) => variant}
  @syntax{(setf (pango:font-description-variant desc) variant)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[variant]{a @symbol{pango:variant} value with the variant type for
    the font description}
  @begin{short}
    The @fun{pango:font-description-variant} function gets the variant field of
    a font description.
  @end{short}
  The @setf{pango:font-description-variant} function sets the variant field.
  The @symbol{pango:variant} value can either be @code{:normal} or
  @code{:small-caps}.
  @see-class{pango:font-description}
  @see-symbol{pango:variant}"
  (desc (g:boxed font-description)))

(export 'font-description-variant)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_weight ()
;;; pango_font_description_set_weight ()
;;; ----------------------------------------------------------------------------

(defun (setf font-description-weight) (weight desc)
  (cffi:foreign-funcall "pango_font_description_set_weight"
                        (g:boxed font-description) desc
                        weight weight
                        :void)
  weight)

(cffi:defcfun ("pango_font_description_get_weight" font-description-weight)
    weight
 #+liber-documentation
 "@version{2024-3-3}
  @syntax{(pango:font-description-weight desc) => weight}
  @syntax{(setf (pango:font-description-weight desc) weight)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[weight]{a @symbol{pango:weight} value or an integer for the weight
    of the font description}
  @begin{short}
    The @fun{pango:font-description-weight} function gets the weight field of a
    font description.
  @end{short}
  The @setf{pango:font-description-weight} function sets the weight field. The
  weight field specifies how bold or light the font should be. In addition to
  the values of the @symbol{pango:weight} enumeration, other intermediate
  numeric values are possible.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq desc (pango:font-description-new))
=> #<PANGO:FONT-DESCRIPTION {1005A11E63@}>
(pango:font-description-weight desc)
=> :NORMAL
(setf (pango:font-description-weight desc) 450)
=> 450
(pango:font-description-weight desc)
=> 450
    @end{pre}
  @end{dictionary}
  @see-class{pango:font-description}
  @see-symbol{pango:weight}"
  (desc (g:boxed font-description)))

(export 'font-description-weight)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_stretch ()
;;; pango_font_description_set_stretch ()
;;; ----------------------------------------------------------------------------

(defun (setf font-description-stretch) (stretch desc)
  (cffi:foreign-funcall "pango_font_description_set_stretch"
                        (g:boxed font-description) desc
                        stretch stretch
                        :void)
  stretch)

(cffi:defcfun ("pango_font_description_get_stretch" font-description-stretch)
    stretch
 #+liber-documentation
 "@version{2024-3-3}
  @syntax{(pango:font-description-stretch desc) => stretch}
  @syntax{(setf (pango:font-description-stretch desc) stretch)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[stretch]{a @symbol{pango:stretch} value for the stretch for the
    font description}
  @begin{short}
    The @fun{pango:font-description} function gets the stretch field of a font
    description.
  @end{short}
  The @setf{pango:font-description-stretch} function sets the stretch field.
  The stretch field specifies how narrow or wide the font should be.
  @see-class{pango:font-description}
  @see-symbol{pango:stretch}"
  (desc (g:boxed font-description)))

(export 'font-description-stretch)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_size ()
;;; pango_font_description_set_size ()
;;; ----------------------------------------------------------------------------

(defun (setf font-description-size) (size desc)
  (cffi:foreign-funcall "pango_font_description_set_size"
                        (g:boxed font-description) desc
                        :int size
                        :void)
  size)

(cffi:defcfun ("pango_font_description_get_size" font-description-size) :int
 #+liber-documentation
 "@version{2024-3-3}
  @syntax{(pango:font-description-size desc) => size}
  @syntax{(setf (pango:font-description-size desc) size)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[size]{an integer with the size of the font in points, scaled by
    the @var{pango:+scale+} constant}
  @begin{short}
    The @fun{pango:font-description-size} function gets the size field of a
    font description in points or device units.
  @end{short}
  The @setf{pango:font-description-size} function sets the size field. This is
  mutually exclusive with the @fun{pango:font-description-set-absolute-size}
  function.

  A size value of @code{(* 10 pango:+scale+)} is a 10 point font. The conversion
  factor between points and device units depends on the system configuration
  and the output device. For screen display, a logical DPI of 96 is common, in
  which case a 10 point font corresponds to a 10 * (96 / 72) = 13.3 pixel font.
  Use the @fun{pango:font-description-set-absolute-size} function if you need a
  particular size in device units.

  You must call the @fun{pango:font-description-size-is-absolute} function
  to find out which is the case. Returns 0 if the size field has not previously
  been set or it has been set to 0 explicitly. Use the
  @fun{pango:font-description-set-fields} function to find out if the field was
  explicitly set or not.
  @see-class{pango:font-description}
  @see-variable{pango:+scale+}
  @see-function{pango:font-description-set-absolute-size}
  @see-function{pango:font-description-size-is-absolute}
  @see-function{pango:font-description-set-fields}"
  (desc (g:boxed font-description)))

(export 'font-description-size)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_absolute_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_set_absolute_size"
                %font-description-set-absolute-size) :void
  (desc (g:boxed font-description))
  (size :double))

(defun font-description-set-absolute-size (desc size)
 #+liber-documentation
 "@version{2024-3-4}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[size]{a number coerced to a double float with the new size, in
    Pango units}
  @begin{short}
    Sets the size field of a font description, in device units.
  @end{short}
  There are @var{pango:+scale+} Pango units in one device unit. For an output
  backend where a device unit is a pixel, a size value of
  @code{(* 10 pango:+scale+)} gives a 10 pixel font.

  This is mutually exclusive with the @fun{pango:font-description-size}
  function which sets the font size in points.
  @see-class{pango:font-description}
  @see-variable{pango:+scale+}
  @see-function{pango:font-description-size}"
  (%font-description-set-absolute-size desc (coerce size 'double-float)))

(export 'font-description-set-absolute-size)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_size_is_absolute ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_get_size_is_absolute"
                font-description-size-is-absolute) :boolean
 #+liber-documentation
 "@version{2024-3-3}
  @argument[desc]{a @class{pango:font-description} instance}
  @begin{return}
    Whether the size for the font description is in points or device units.
  @end{return}
  @begin{short}
    Determines whether the size of the font is in points (not absolute) or
    device units (absolute).
  @end{short}
  See the @fun{pango:font-description-size} and
  @fun{pango:font-description-set-absolute-size} functions. Use the
  @fun{pango:font-description-set-fields} function to find out if the size
  field of the font description was explicitly set or not.
  @see-class{pango:font-description}
  @see-function{pango:font-description-size}
  @see-function{pango:font-description-set-absolute-size}
  @see-function{pango:font-description-set-fields}"
  (desc (g:boxed font-description)))

(export 'font-description-size-is-absolute)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_gravity ()
;;; pango_font_description_set_gravity ()
;;; ----------------------------------------------------------------------------

(defun (setf font-description-gravity) (gravity desc)
  (cffi:foreign-funcall "pango_font_description_set_gravity"
                        (g:boxed font-description) desc
                        gravity gravity
                        :void)
  gravity)

(cffi:defcfun ("pango_font_description_get_gravity" font-description-gravity)
    gravity
 #+liber-documentation
 "@version{2024-3-3}
  @syntax{(pango:font-description-gravity desc) => gravity}
  @syntax{(setf (pango:font-description-gravity desc) gravity)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[gravity]{a @symbol{pango:gravity} value for the gravity for the
    font description}
  @begin{short}
    The @fun{pango:font-description-gravity} function gets the gravity field
    of a font description.
  @end{short}
  The @setf{pango:font-description-gravity} function sets the gravity field.
  The gravity field specifies how the glyphs should be rotated. If gravity is
  @code{:auto}, this actually unsets the gravity mask on the font description.

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
;;; ----------------------------------------------------------------------------

(defun (setf font-description-variations) (variations desc)
  (cffi:foreign-funcall "pango_font_description_set_variations"
                        (g:boxed font-description) desc
                        :string variations
                        :void)
  variations)

(cffi:defcfun ("pango_font_description_get_variations"
                font-description-variations) :string
 #+liber-documentation
 "@version{2024-3-3}
  @syntax{(pango:font-description-variations desc) => variations}
  @syntax{(setf (pango:font-description-variations desc) variations)}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[variations]{a string representing the variations}
  @begin{short}
    The @fun{pango:font-description-variations} function gets the variations
    field of a font description.
  @end{short}
  The @setf{pango:font-description-variations} function sets the variations
  field. OpenType font variations allow to select a font instance by specifying
  values for a number of axes, such as width or weight.

  The format of the variations string is @code{AXIS1=VALUE, AXIS2=VALUE ...,}
  with each @code{AXIS} a 4 character tag that identifies a font axis, and each
  @code{VALUE} a floating point number. Unknown axes are ignored, and values
  are clamped to their allowed range.

  Pango does not currently have a way to find supported axes of a font. Both
  the HarfBuzz or FreeType libraries have API for this.
  @see-class{pango:font-description}"
  (desc (g:boxed font-description)))

(export 'font-description-variations)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_set_variations_static ()        not implemented
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_font_description_set_variations_static"
                font-description-set-variations-static) :void
 #+liber-documentation
 "@version{#2021-1-7}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[variations]{a string representing the variations}
  @begin{short}
    Like the @fun{pango:font-description-variations} function, except that no
    copy of variations is made.
  @end{short}
  The caller must make sure that the string passed in stays around until
  @arg{desc} has been freed or the name is set again. This function can be used
  if variations is a static string such as a C string literal, or if @arg{desc}
  is only needed temporarily.
  @see-class{pango:font-description}
  @see-function{pango:font-description-variations}"
  (desc (g:boxed font-description))
  (variations :string))

;;; ----------------------------------------------------------------------------
;;; pango_font_description_get_set_fields ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_get_set_fields"
                font-description-set-fields) font-mask
 #+liber-documentation
 "@version{2024-3-4}
  @argument[desc]{a @class{pango:font-description} instance}
  @begin{return}
    The list with the @symbol{pango:font-mask} values set corresponding to the
    fields in @arg{desc} that have been set.
  @end{return}
  @begin{short}
    Determines which fields in a font description have been set.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(pango:font-description-from-string \"Sans Bold 16\")
=> #<PANGO:FONT-DESCRIPTION {10077E5773@}>
(pango:font-description-set-fields *)
=> (:FAMILY :STYLE :VARIANT :WEIGHT :STRETCH :SIZE)
    @end{pre}
  @end{dictionary}
  @see-class{pango:font-description}
  @see-symbol{pango:font-mask}"
  (desc (g:boxed font-description)))

(export 'font-description-set-fields)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_unset_fields ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_unset_fields"
                font-description-unset-fields) :void
 #+liber-documentation
 "@version{2024-3-4}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[unset]{a list with the @symbol{pango:font-mask} values in
    @arg{desc} to unset}
  @begin{short}
    Unsets some of the fields in a font description.
  @end{short}
  The unset fields will get back to their default values.
  @see-class{pango:font-description}
  @see-symbol{pango:font-mask}"
  (desc (g:boxed font-description))
  (unset font-mask))

(export 'font-description-unset-fields)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_merge ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_merge" font-description-merge) :void
 #+liber-documentation
 "@version{2024-3-4}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[merge]{a @class{pango:font-description} instance to merge from,
    or @code{nil}}
  @argument[replace]{if @em{true}, replace fields in @arg{desc} with the
    corresponding values from @arg{merge}, even if they are already exist}
  @begin{short}
    Merges the fields that are set in @arg{merge} into the fields in
    @arg{desc}.
  @end{short}
  If @arg{replace} is @em{false}, only fields in @arg{desc} that are not
  already set are affected. If @em{true}, then fields that are already set will
  be replaced as well.

  If @arg{merge} is @code{nil}, this function performs nothing.
  @see-class{pango:font-description}"
  (desc (g:boxed font-description))
  (merge (g:boxed font-description))
  (replace :boolean))

(export 'font-description-merge)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_merge_static ()                 not implemented
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_font_description_merge_static"
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
    Like the @fun{pango:font-description-merge} function, but only a shallow
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

(cffi:defcfun ("pango_font_description_better_match"
                font-description-better-match) :boolean
 #+liber-documentation
 "@version{#2023-8-27}
  @argument[desc]{a @class{pango:font-description} instance}
  @argument[old]{a @class{pango:font-description} instance, or @code{nil}}
  @argument[new]{a @class{pango:font-description} instance}
  @return{@em{True} if @arg{new} is a better match.}
  @begin{short}
    Determines if the style attributes of @arg{new} are a closer match for
    @arg{desc} than those of @arg{old} are, or if @arg{old} is @code{nil},
    determines if @arg{new} is a match at all.
  @end{short}
  Approximate matching is done for weight and style. Other style attributes
  must match exactly. Style attributes are all attributes other than family and
  size-related attributes. Approximate matching for style considers
  @code{:oblique} and @code{:italic} as matches, but not as good a match as
  when the styles are equal.

  Note that @arg{old} must match @arg{desc}.
  @see-class{pango:font-description}"
  (desc (g:boxed font-description))
  (old (g:boxed font-description))
  (new (g:boxed font-description)))

(export 'font-description-better-match)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_from_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_from_string"
                font-description-from-string) (g:boxed font-description)
 #+liber-documentation
 "@version{2024-3-4}
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
  @begin[Example]{dictionary}
    A typical example is:
    @begin{pre}
(pango:font-description-from-string \"Cantarell Italic Light 15 @@wght=200\")
=> #<PANGO:FONT-DESCRIPTION {10077E7DC3@}>
(pango:font-description-to-string *)
\"Cantarell Light Italic 15 @@wght=200\"
    @end{pre}
  @end{dictionary}
  @see-class{pango:font-description}
  @see-function{pango:font-description-to-string}"
  (str :string))

(export 'font-description-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_font_description_to_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_description_to_string"
                font-description-to-string) :string
 #+liber-documentation
 "@version{2024-3-4}
  @argument[desc]{a @class{pango:font-description} instance}
  @return{A new string with a representation of a font description.}
  @begin{short}
    Creates a string representation of a font description.
  @end{short}
  See the @fun{pango:font-description-from-string} function for a description
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

(cffi:defcfun ("pango_font_description_to_filename"
                font-description-to-filename) :string
 #+liber-documentation
 "@version{2024-3-4}
  @argument[desc]{a @class{pango:font-description} instance}
  @return{The string with a filname.}
  @begin{short}
    Creates a filename representation of a font description.
  @end{short}
  The filename is identical to the result from calling the
  @fun{pango:font-description-to-string} function, but with underscores instead
  of characters that are untypical in filenames, and in lower case only.
  @begin[Example]{dictionary}
    @begin{pre}
(pango:font-description-from-string \"Sans Bold 16\")
=> #<PANGO:FONT-DESCRIPTION {1007EF4DF3@}>
(pango:font-description-to-filename *)
=> \"sans_bold_16\"
    @end{pre}
  @end{dictionary}
  @see-class{pango:font-description}
  @see-function{pango:font-description-to-string}"
  (desc (g:boxed font-description)))

(export 'font-description-to-filename)

;;; --- End of file pango.font-description.lisp --------------------------------
