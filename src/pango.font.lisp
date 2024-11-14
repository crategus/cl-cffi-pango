;;; ----------------------------------------------------------------------------
;;; pango.font.lisp
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
;;; Fonts
;;;
;;;     Structures representing abstract fonts
;;;
;;; Types and Values
;;;
;;;     PangoFont
;;;     PangoFontFamily
;;;     PangoFontFace
;;;     PangoFontMap
;;;     PangoFontset
;;;
;;; Functions
;;;
;;;     pango_font_describe
;;;     pango_font_describe_with_absolute_size
;;;     pango_font_get_face
;;;     pango_font_get_coverage
;;;     pango_font_has_char
;;;     pango_font_get_glyph_extents
;;;     pango_font_get_metrics
;;;     pango_font_get_font_map
;;;     pango_font_get_features                            not implemented
;;;     pango_font_get_hb_font                             not implemented
;;;     pango_font_get_languages                           Since 1.50
;;;     pango_font_serialize                               Since 1.50
;;;     pango_font_deserialize                             Since 1.50
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
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoFont
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "PangoFont" font
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_font_get_type")
  nil)

#+liber-documentation
(setf (documentation 'font 'type)
 "@version{2024-3-5}
  @begin{short}
    The @class{pango:font} class is used to represent a font in a
    rendering-system-independent matter.
  @end{short}
  @see-class{pango:font-map}")

;;; ----------------------------------------------------------------------------
;;; PangoFontFamily
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "PangoFontFamily" font-family
  (:superclass g:object
    :export t
    :interfaces ("GListModel")
    :type-initializer "pango_font_family_get_type")
  (#+pango-1-52
   (is-monospace
    font-family-is-monospace
    "is-monospace" "gboolean" t nil)
   #+pango-1-52
   (is-variable
    font-family-is-variable
    "is-variable" "gboolean" t nil)
   (item-type
    font-family-item-type
    "item-type" "GType" t nil)
   (n-items
    font-family-n-items
    "n-items" "guint" t nil)
   #+pango-1-52
   (name
    font-family-name
    "name" "gchararray" t nil)))

#+liber-documentation
(setf (documentation 'font-family 'type)
 "@version{2024-5-25}
  @begin{short}
    The @class{pango:font-family} class is used to represent a family of
    related font faces.
  @end{short}
  The faces in a family share a common design, but differ in slant, weight,
  width and other aspects.
  @see-slot{pango:font-family-is-monospace}
  @see-slot{pango:font-family-is-variable}
  @see-slot{pango:font-family-item-type}
  @see-slot{pango:font-family-n-items}
  @see-slot{pango:font-family-name}
  @see-class{pango:font}
  @see-class{pango:font-face}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- pango:font-family-is-monospace -----------------------------------------

#+(and pango-1-52 liber-documentation)
(setf (documentation (liber:slot-documentation "is-monospace" 'font-family) t)
 "The @code{is-monospace} property of type @code{:boolean} (Read) @br{}
  Whether this is a monospace font. Since 1.52 @br{}
  Default value: @em{false}")

#+(and pango-1-52 liber-documentation)
(setf (liber:alias-for-function 'font-family-is-monospace)
      "Accessor"
      (documentation 'font-family-is-monospace 'function)
 "@version{2024-5-25}
  @syntax{(pango:font-family-is-monospace object) => setting}
  @argument[object]{a @class{pango:font-family} object}
  @argument[setting]{@em{true} if @arg{object} is monospace}
  @begin{short}
    A monospace font is a font designed for text display where the characters
    form a regular grid.
  @end{short}
  For Western languages this would mean that the advance width of all characters
  are the same, but this categorization also includes Asian fonts which include
  double-width characters. The @code{g_unichar_iswide()} function returns a
  result that indicates whether a character is typically double-width in a
  monospace font.

  The best way to find out the grid-cell size is to call the
  @fun{pango:font-metrics-approximate-digit-width} function, since the results
  of the @fun{pango:font-metrics-approximate-char-width} function may be
  affected by double-width characters.

  Since 1.52
  @see-class{pango:font-family}
  @see-function{pango:font-metrics-approximate-digit-width}
  @see-function{pango:font-metrics-approximate-char-width}")

;;; --- pango:font-family-is-variable ------------------------------------------

#+(and pango-1-52 liber-documentation)
(setf (documentation (liber:slot-documentation "is-variable" 'font-family) t)
 "The @code{is-variable} property of type @code{:boolean} (Read) @br{}
  Whether this is a variable font. Since 1.52 @br{}
  Default value: @em{false}")

#+(and pango-1-52 liber-documentation)
(setf (liber:alias-for-function 'font-family-is-variable)
      "Accessor"
      (documentation 'font-family-is-variable 'function)
 "@version{2024-5-25}
  @syntax{(pango:font-family-is-variable object) => setting}
  @argument[object]{a @class{pango:font-family} object}
  @argument[setting]{@em{true} if @arg{object} is variable}
  @begin{short}
    A variable font is a font which has axes that can be modified to produce
    different faces.
  @end{short}

  Since 1.52
  @see-class{pango:font-family}")

;;; --- pango:font-family-item-type --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item-type" 'font-family) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items contained in the list.")

#+liber-documentation
(setf (liber:alias-for-function 'font-family-item-type)
      "Accessor"
      (documentation 'font-family-item-type 'function)
 "@version{2024-3-5}
  @syntax{(pango:font-family-item-type object) => type}
  @argument[object]{a @class{pango:font-family} object}
  @argument[type]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[pango:font-family]{item-type} slot of the
    @class{pango:font-family} class.
  @end{short}
  The @fun{pango:font-family-item-type} function returns the type of items
  contained in the list.
  @begin[Warning]{dictionary}
    This function does not return a @code{GType} but a pointer with an address
    value that is the @code{GType}. This is possibly a bug in the Pango library.
    You get the correct @code{GType} with the @fun{g:list-model-item-type}
    function of the @class{g:list-model} interface.
  @end{dictionary}
  @see-class{pango:font-family}
  @see-class{g:type-t}")

;;; --- pango:font-family-n-items ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-items" 'font-family) t)
 "The @code{n-items} property of type @code{:uint} (Read) @br{}
  The number of items contained in the list. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'font-family-n-items)
      "Accessor"
      (documentation 'font-family-n-items 'function)
 "@version{2024-3-5}
  @syntax{(pango:font-family-n-items object) => n-items}
  @argument[object]{a @class{pango:font-family} object}
  @argument[n-items]{an unsigned integer}
  @begin{short}
    Accessor of the @slot[pango:font-family]{n-items} slot of the
    @class{pango:font-family} class.
  @end{short}
  The @fun{pango:font-family-n-items} function returns the number of items
  contained in the list.
  @see-class{pango:font-family}")

;;; --- pango:font-family-name -------------------------------------------------

#+(and pango-1-52 liber-documentation)
(setf (documentation (liber:slot-documentation "name" 'font-family) t)
 "The @code{name} property of type @code{:string} (Read) @br{}
  The name of the the font family. Since 1.52 @br{}
  Default value: @code{nil}")

#+(and pango-1-52 liber-documentation)
(setf (liber:alias-for-function 'font-family-name)
      "Accessor"
      (documentation 'font-family-name 'function)
 "@version{2024-5-25}
  @syntax{(pango:font-family-name object) => name}
  @argument[object]{a @class{pango:font-family} object}
  @argument[name]{a string with the name of @arg{object}}
  @begin{short}
    Gets the name of the font family.
  @end{short}
  The name is unique among all fonts for the font backend and can be used in a
  @class{pango:font-description} instance to specify that a face from this
  family is desired.

  Since 1.52
  @see-class{pango:font-family}
  @see-class{pango:font-description}")

;;; ----------------------------------------------------------------------------
;;; PangoFontFace
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "PangoFontFace" font-face
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "pango_font_face_get_type")
  nil)

#+liber-documentation
(setf (documentation 'font-face 'type)
 "@version{2024-3-5}
  @begin{short}
    The @class{pango:font-face} class is used to represent a group of fonts
    with the same family, slant, weight, width, but varying sizes.
  @end{short}
  @see-class{pango:font}
  @see-class{pango:font-family}")

;;; ----------------------------------------------------------------------------
;;; PangoFontMap
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "PangoFontMap" font-map
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_font_map_get_type")
  ((item-type
    font-map-item-type
    "item-type" "GType" t nil)
   (n-items
    font-map-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'font-map 'type)
 "@version{2024-3-6}
  @begin{short}
    The @class{pango:font-map} class represents the set of fonts available for
    a particular rendering system.
  @end{short}
  This is a virtual object with implementations being specific to particular
  rendering systems.
  @see-slot{pango:font-map-item-type}
  @see-slot{pango:font-map-n-items}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- pango:font-map-item-type -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item-type" 'font-map) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items contained in the list.")

#+liber-documentation
(setf (liber:alias-for-function 'font-map-item-type)
      "Accessor"
      (documentation 'font-map-item-type 'function)
 "@version{2024-3-5}
  @syntax{(pango:font-map-item-type object) => type}
  @argument[object]{a @class{pango:font-map} object}
  @argument[type]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[pango:font-map]{item-type} slot of the
    @class{pango:font-map} class.
  @end{short}
  The @fun{pango:font-map-item-type} function returns the type of items
  contained in the list.
  @begin[Warning]{dictionary}
    This function does not return a @code{GType} but a pointer with an address
    value that is the @code{GType}. This is possibly a bug in the Pango library.
    You get the correct @code{GType} with the @fun{g:list-model-item-type}
    function of the @class{g:list-model} interface.
  @end{dictionary}
  @see-class{pango:font-map}
  @see-class{g:type-t}")

;;; --- pango:font-map-n-items -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-items" 'font-map) t)
 "The @code{n-items} property of type @code{:uint} (Read) @br{}
  The number of items contained in the list. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'font-map-n-items)
      "Accessor"
      (documentation 'font-map-n-items 'function)
 "@version{2024-3-5}
  @syntax{(pango:font-map-n-items object) => n-items}
  @argument[object]{a @class{pango:font-map} object}
  @argument[n-items]{an unsigned integer}
  @begin{short}
    Accessor of the @slot[pango:font-map]{n-items} slot of the
    @class{pango:font-map} class.
  @end{short}
  The @fun{pango:font-map-n-items} function returns the number of items
  contained in the list.
  @see-class{pango:font-map}")

;;; ----------------------------------------------------------------------------
;;; PangoFontset
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "PangoFontset" fontset
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_fontset_get_type")
  nil)

#+liber-documentation
(setf (documentation 'fontset 'type)
 "@version{2024-3-4}
  @begin{short}
    The @class{pango:fontset} object represents a set of @class{pango:font}
    objects to use when rendering text.
  @end{short}
  It is the result of resolving a @class{pango:font-description} instance
  against a particular @class{pango:context} object. It has operations for
  finding the component font for a particular Unicode character, and for
  finding a composite set of metrics for the entire fontset.
  @begin[Example]{dictionary}
    Code fragement for getting the fontset:
    @begin{pre}
(let* ((fontmap (pango:cairo-font-map-default))
       (context (pango:font-map-create-context fontmap))
       (desc (pango:font-description-from-string \"Sans Italic 12\"))
       (lang (pango:language-default))
       (fontset (pango:font-map-load-fontset fontmap context desc lang)))
  fontset)
=> #<PANGO:FONTSET {10026D1863@}>
    @end{pre}
  @end{dictionary}
  @see-class{pango:font}
  @see-class{pango:context}
  @see-class{pango:font-description}")

;;; ----------------------------------------------------------------------------
;;; pango_font_describe
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_describe" font-describe) (g:boxed font-description)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @return{A newly allocated @class{pango:font-description} instance.}
  @begin{short}
    Returns a description of the font, with font size set in points.
  @end{short}
  Use the @fun{pango:font-describe-with-absolute-size} function if you want the
  font size in device units.
  @see-class{pango:font}
  @see-class{pango:font-description}
  @see-function{pango:font-describe-with-absolute-size}"
  (font (g:object font)))

(export 'font-describe)

;;; ----------------------------------------------------------------------------
;;; pango_font_describe_with_absolute_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_describe_with_absolute_size"
                font-describe-with-absolute-size) (g:boxed font-description)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @return{A newly allocated @class{pango:font-description} instance.}
  @begin{short}
    Returns a description of the font, with absolute font size set, in device
    units.
  @end{short}
  Use the @fun{pango:font-describe} function if you want the font size in
  points.
  @see-class{pango:font}
  @see-class{pango:font-description}"
  (font (g:object font)))

(export 'font-describe-with-absolute-size)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_face
;;; ----------------------------------------------------------------------------

#+pango-1-46
(cffi:defcfun ("pango_font_get_face" font-face) (g:object font-face)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @return{The @class{pango:font-face} object.}
  @begin{short}
    Gets the Pango font face to which the given font belongs.
  @end{short}

  Since 1.46
  @see-class{pango:font}
  @see-class{pango:font-face}"
  (font (g:object font)))

#+pango-1-46
(export 'font-face)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_coverage
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_get_coverage" font-coverage) (g:object coverage)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @argument[language]{a @class{pango:language} instance}
  @return{A newly allocated @class{pango:coverage} object.}
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
;;; pango_font_has_char
;;; ----------------------------------------------------------------------------

#+pango-1-44
(cffi:defcfun ("pango_font_has_char" font-has-char) :boolean
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @argument[char]{a character or a char code with a Unicode character}
  @return{Returns whether the font provides a glyph for the given character.}
  @begin{short}
    Returns @em{true} if @arg{font} can render the given character.
  @end{short}

  Since 1.44
  @see-class{pango:font}"
  (font (g:object font))
  (char g:unichar))

#+pango-1-44
(export 'font-has-char)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_glyph_extents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_get_glyph_extents" %font-glyph-extents) :void
  (font (g:object font))
  (glyph glyph)
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun font-glyph-extents (font glyph ink logical)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @argument[glyph]{an unsigned integer with the glyph index}
  @argument[ink]{a @symbol{pango:rectangle} instance used to store the extents
    of the glyph as drawn or @code{nil} to indicate that the result is not
    needed}
  @argument[logical]{a @symbol{pango:rectangle} instance used to store the
    logical extents of the glyph or @code{nil} to indicate that the result is
    not needed}
  @begin{short}
    Gets the logical and ink extents of a glyph within a font.
  @end{short}
  The coordinate system for each rectangle has its origin at the base line and
  horizontal origin of the character with increasing coordinates extending to
  the right and down. The @fun{pango:ascent}, @fun{pango:descent},
  @fun{pango:lbearing}, and @fun{pango:rbearing} functions can be used to
  convert from the extents rectangle to more traditional font metrics. The
  units of the rectangles are in @code{(/ 1 pango:+scale+)} of a device unit.

  If the @arg{font} argument is @code{nil}, this function gracefully sets some
  sane values in the output variables and returns.
  @see-class{pango:font}
  @see-symbol{pango:glyph}"
  (%font-glyph-extents font
                       glyph
                       (or ink (cffi:null-pointer))
                       (or logical (cffi:null-pointer))))

(export 'font-glyph-extents)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_metrics
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_get_metrics" font-metrics)
    (g:boxed font-metrics :return)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @argument[language]{a @class{pango:language} instance with the language tag
    used to determine which script to get the metrics for, or @code{nil} to
    indicate to get the metrics for the entire font}
  @begin{return}
    The @class{pango:font-metrics} instance.
  @end{return}
  @begin{short}
    Gets overall metric information for a font.
  @end{short}
  Since the metrics may be substantially different for different scripts, a
  language tag can be provided to indicate that the metrics should be retrieved
  that correspond to the script(s) used by that language.

  If the @arg{font} argument is @code{nil}, this function gracefully sets some
  sane values in the output variables and returns.
  @see-class{pango:font}
  @see-class{pango:language}
  @see-class{pango:font-metrics}"
  (font (g:object font))
  (language (g:boxed language)))

(export 'font-metrics)

;;; ----------------------------------------------------------------------------
;;; pango_font_get_font_map
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_get_font_map" font-font-map) (g:object font-map)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object, or @code{nil}}
  @begin{return}
    The @class{pango:font-map} object for the font, or @code{nil} if
    @arg{font} is @code{nil}
  @end{return}
  @begin{short}
    Gets the font map for which the font was created.
  @end{short}
  @see-class{pango:font}
  @see-class{pango:font-map}"
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
;;; pango_font_get_languages
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_font_get_languages" %font-languages) :pointer
  (font (g:object font)))

#+pango-1-50
(defun font-languages (font)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @return{The list with the @class{pango:language} instances for @arg{font}.}
  @begin{short}
    Returns the languages that are supported by the font backend.
  @end{short}
  If the font backend does not provide this information, @code{nil} is returned.
  For the @code{fontconfig} backend, this corresponds to the @code{FC_LANG}
  member of the @code{FcPattern}.

  Since 1.50
  @see-class{pango:font}
  @see-class{pango:language}"
  (let ((ptr (%font-languages font)))
    (unless (cffi:null-pointer-p ptr)
      (iter (for i from 0)
            (for lang-ptr = (cffi:mem-aptr ptr :pointer i))
            (until (cffi:null-pointer-p (cffi:mem-ref lang-ptr :pointer)))
            (collect (cffi:convert-from-foreign (cffi:mem-ref lang-ptr :pointer)
                                                '(g:boxed language)))))))

#+pango-1-50
(export 'font-languages)

;;; ----------------------------------------------------------------------------
;;; pango_font_serialize
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_font_serialize" font-serialize) (g:boxed g:bytes :return)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[font]{a @class{pango:font} object}
  @begin{return}
    The @class{g:bytes} instance containing the serialized form of @arg{font}.
  @end{return}
  @begin{short}
    Serializes the font in a way that can be uniquely identified.
  @end{short}
  There are no guarantees about the format of the output across different
  versions of Pango. The intended use of this function is testing, benchmarking
  and debugging. The format is not meant as a permanent storage format.

  To recreate a font from its serialized form, use the
  @fun{pango:font-deserialize} function.

  Since 1.50
  @see-class{pango:font}
  @see-class{g:bytes}
  @see-function{pango:font-deserialize}"
  (font (g:object font)))

#+pango-1-50
(export 'font-serialize)

;;; ----------------------------------------------------------------------------
;;; pango_font_deserialize
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_font_deserialize" %font-deserialize) (g:object font)
  (context (g:object context))
  (bytes (g:boxed g:bytes))
  (err :pointer))

#+pango-1-50
(defun font-deserialize (context bytes)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[context]{a @class{pango:context} object}
  @argument[bytes]{a @class{g:bytes} object containing the data}
  @return{The new @class{pango:font} object.}
  @begin{short}
    Loads data previously created via the @fun{pango:font-serialize} function.
  @end{short}
  For a discussion of the supported format, see that function.

  Since 1.50
  @begin[Note]{dictionary}
    To verify that the returned font is identical to the one that was
    serialized, you can compare @arg{bytes} to the result of serializing the
    font again.
  @end{dictionary}
  @see-class{pango:font}
  @see-class{pango:context}
  @see-class{g:bytes}
  @see-function{pango:font-serialize}"
  (glib:with-g-error (err)
    (%font-deserialize context bytes err)))

#+pango-1-50
(export 'font-deserialize)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_get_name
;;; ----------------------------------------------------------------------------

#-pango-1-52
(cffi:defcfun ("pango_font_family_get_name" font-family-name) :string
 #+liber-documentation
 "@version{2024-3-5}
  @argument[family]{a @class{pango:font-family} object}
  @return{The string with the name of @arg{family}.}
  @begin{short}
    Gets the name of the font family.
  @end{short}
  The name is unique among all fonts for the font backend and can be used in a
  @class{pango:font-description} instance to specify that a face from this
  family is desired.
  @see-class{pango:font-family}
  @see-class{pango:font-description}"
  (family (g:object font-family)))

#-pango-1-52
(export 'font-family-name)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_is_monospace
;;; ----------------------------------------------------------------------------

#-pango-1-52
(cffi:defcfun ("pango_font_family_is_monospace" font-family-is-monospace)
    :boolean
 #+liber-documentation
 "@version{2024-3-5}
  @argument[family]{a @class{pango:font-family} object}
  @return{@em{True} if @arg{family} is monospace.}
  @begin{short}
    A monospace font is a font designed for text display where the characters
    form a regular grid.
  @end{short}
  For Western languages this would mean that the advance width of all characters
  are the same, but this categorization also includes Asian fonts which include
  double-width characters. The @code{g_unichar_iswide()} function returns a
  result that indicates whether a character is typically double-width in a
  monospace font.

  The best way to find out the grid-cell size is to call the
  @fun{pango:font-metrics-approximate-digit-width} function, since the results
  of the @fun{pango:font-metrics-approximate-char-width} function may be
  affected by double-width characters.
  @see-class{pango:font-family}
  @see-function{pango:font-metrics-approximate-digit-width}
  @see-function{pango:font-metrics-approximate-char-width}"
  (family (g:object font-family)))

#-pango-1-52
(export 'font-family-is-monospace)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_is_variable
;;; ----------------------------------------------------------------------------

#+(and pango-1-44 (not pango-1-52))
(cffi:defcfun ("pango_font_family_is_variable" font-family-is-variable) :boolean
 #+liber-documentation
 "@version{2024-3-5}
  @argument[family]{a @class{pango:font-family} object}
  @return{@em{True} if @arg{family} is variable.}
  @begin{short}
    A variable font is a font which has axes that can be modified to produce
    different faces.
  @end{short}

  Since 1.44
  @see-class{pango:font-family}"
  (family (g:object font-family)))

#+(and pango-1-44 (not pango-1-52))
(export 'font-family-is-variable)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_list_faces
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_family_list_faces" %font-family-list-faces) :void
  (family (g:object font-family))
  (faces :pointer)
  (n-faces (:pointer :int)))

(defun font-family-list-faces (family)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[family]{a @class{pango:font-family} object}
  @begin{return}
    The list of @class{pango:font-face} objects, or @code{nil}.
  @end{return}
  @begin{short}
    Lists the different font faces that make up @arg{family}.
  @end{short}
  The faces in a family share a common design, but differ in slant, weight,
  width and other aspects. You get the number of font faces contained in the
  font family with the @fun{pango:font-family-n-items} function.
  @see-class{pango:font-family}
  @see-class{pango:font-face}
  @see-function{pango:font-family-n-items}"
  (cffi:with-foreign-objects ((faces-ptr :pointer) (n-faces :int))
    (%font-family-list-faces family faces-ptr n-faces)
    (iter (with faces-ar = (cffi:mem-ref faces-ptr :pointer))
          (for i from 0 below (cffi:mem-ref n-faces :int))
          (collect
              (cffi:convert-from-foreign (cffi:mem-aref faces-ar :pointer i)
                                         'g:object))
          (finally (glib:free faces-ar)))))

(export 'font-family-list-faces)

;;; ----------------------------------------------------------------------------
;;; pango_font_family_get_face
;;; ----------------------------------------------------------------------------

#+pango-1-46
(cffi:defcfun ("pango_font_family_get_face" %font-family-face)
    (g:object font-face)
  (family (g:object font-family))
  (name :string))

(defun font-family-face (family name)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[family]{a @class{pango:font-family} object}
  @argument[name]{a string with the name of a fpmt face, if @arg{name} is
    @code{nil}, the default font face of the font family is returned}
  @begin{return}
    The @class{pango:font-face} object, or @code{nil} if no face with the given
    name exists.
  @end{return}
  @begin{short}
    Gets the Pango font face of the given font family with the given @arg{name}.
  @end{short}

  Since 1.46
  @see-class{pango:font-family}
  @see-class{pango:font-face}"
  (%font-family-face family (or name (cffi:null-pointer))))

#+pango-1-46
(export 'font-family-face)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_get_face_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_face_get_face_name" font-face-face-name) :string
 #+liber-documentation
 "@version{2024-3-5}
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
;;; pango_font_face_list_sizes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_face_list_sizes" %font-face-list-sizes) :void
  (face (g:object font-face))
  (sizes (:pointer :int))
  (n-sizes (:pointer :int)))

(defun font-face-list-sizes (face)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[face]{a @class{pango:font-face} object}
  @return{The list of integer with the available sizes for a font.}
  @begin{short}
    List the available sizes for a font.
  @end{short}
  This is only applicable to bitmap fonts. For scalable fonts, returns
  @code{nil}. The sizes returned are in Pango units and are sorted in ascending
  order.
  @see-class{pango:font-face}"
  (cffi:with-foreign-objects ((sizes-ptr :pointer) (n-sizes :int))
    (%font-face-list-sizes face sizes-ptr n-sizes)
    (iter (with sizes-ar = (cffi:mem-ref sizes-ptr :pointer))
          (for i from 0 below (cffi:mem-ref n-sizes :int))
          (collect (cffi:mem-aref sizes-ar :int i))
          (finally (glib:free sizes-ar)))))

(export 'font-face-list-sizes)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_describe
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_face_describe" font-face-describe)
    (g:boxed font-description :return)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[face]{a @class{pango:font-face} object}
  @begin{return}
    A newly created @class{pango:font-description} instance holding the
    description of @arg{face}.
  @end{return}
  @begin{short}
    Returns the family, style, variant, weight and stretch of a font face.
  @end{short}
  The size field of the resulting font description will be unset.
  @see-class{pango:font-face}
  @see-class{pango:font-description}"
  (face (g:object font-face)))

(export 'font-face-describe)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_is_synthesized
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_face_is_synthesized" font-face-is-synthesized)
    :boolean
 #+liber-documentation
 "@version{2024-3-5}
  @argument[face]{a @class{pango:font-face} object}
  @return{The boolean whether @arg{face} is synthesized.}
  @begin{short}
    Returns whether a font face is synthesized by the underlying font rendering
    engine from another face, perhaps by shearing, emboldening, or lightening
    it.
  @end{short}
  @see-class{pango:font-face}"
  (face (g:object font-face)))

(export 'font-face-is-synthesized)

;;; ----------------------------------------------------------------------------
;;; pango_font_face_get_family
;;; ----------------------------------------------------------------------------

#+pango-1-46
(cffi:defcfun ("pango_font_face_get_family" font-face-family)
    (g:object font-family)
 #+liber-documentation
 "@version{2024-3-5}
  @argument[face]{a @class{pango:font-face} object}
  @return{The @class{pango:font-family} object.}
  @begin{short}
    Gets the Pango font family that the font face belongs to.
  @end{short}

  Since 1.46
  @see-class{pango:font-face}
  @see-class{pango:font-family}"
  (face (g:object font-face)))

#+pango-1-46
(export 'font-face-family)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_create_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_map_create_context" font-map-create-context)
    (g:object context)
 #+liber-documentation
 "@version{2024-3-6}
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
  it's own way of create a Pango context. For instance, the GTK toolkit has the
  @fun{gtk:widget-pango-context} function. Use this instead.
  @see-class{pango:font-map}
  @see-class{pango:context}
  @see-function{pango:context-new}
  @see-function{pango:context-font-map}
  @see-function{gtk:widget-pango-context}"
  (fontmap (g:object font-map)))

(export 'font-map-create-context)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_load_font
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_map_load_font" font-map-load-font) (g:object font)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[fontmap]{a @class{pango:font-map} object}
  @argument[context]{a @class{pango:context} object the font will be used
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
;;; pango_font_map_load_fontset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_map_load_fontset" font-map-load-fontset)
    (g:object fontset)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[fontmap]{a @class{pango:font-map} object}
  @argument[context]{a @class{pango:context} object the font will be used
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
  @see-class{pango:language}
  @see-class{pango:font-set}"
  (fontmap (g:object font-map))
  (context (g:object context))
  (desc (g:boxed font-description))
  (language (g:boxed language)))

(export 'font-map-load-fontset)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_list_families
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_map_list_families" %font-map-list-families) :void
  (fontmap (g:object font-map))
  (families :pointer)
  (n-families (:pointer :int)))

(defun font-map-list-families (fontmap)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[fontmap]{a @class{pango:font-map} object}
  @begin{return}
    A list of @class{pango:font-family} objects, or @code{nil}.
  @end{return}
  @begin{short}
    List all families for a font map.
  @end{short}
  @see-class{pango:font-map}
  @see-class{pango:font-family}"
  (cffi:with-foreign-objects ((families-ptr :pointer) (n-families :int))
    (%font-map-list-families fontmap families-ptr n-families)
    (iter (with families-ar = (cffi:mem-ref families-ptr :pointer))
          (for i from 0 below (cffi:mem-ref n-families :int))
          (collect (cffi:convert-from-foreign
                           (cffi:mem-aref families-ar :pointer i)
                           'g:object))
          (finally (glib:free families-ar)))))

(export 'font-map-list-families)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_get_family
;;; ----------------------------------------------------------------------------

#+pango-1-46
(cffi:defcfun ("pango_font_map_get_family" font-map-family)
    (g:object font-family)
 #+liber-documentation
 "@version{2024-3-6}
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
;;; pango_font_map_get_serial
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_map_get_serial" font-map-serial) :uint
 #+liber-documentation
 "@version{2024-3-6}
  @argument[fontmap]{a @class{pango:font-map} object}
  @return{The unsigned integer with the current serial number of @arg{fontmap}.}
  @begin{short}
    Returns the current serial number of @arg{fontmap}.
  @end{short}
  The serial number is initialized to a small number larger than zero when a
  new font map is created and is increased whenever the font map is changed. It
  may wrap, but will never have the value 0. Since it can wrap, never compare
  it with \"less than\", always use \"not equals\".

  The font map can only be changed using backend-specific API, like changing
  font map resolution.

  This can be used to automatically detect changes to a Pango font map, like in
  a @class{pango:context} object.
  @see-class{pango:font-map}
  @see-class{pango:context}"
  (fontmap (g:object font-map)))

(export 'font-map-serial)

;;; ----------------------------------------------------------------------------
;;; pango_font_map_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_map_changed" font-map-changed) :void
 #+liber-documentation
 "@version{2024-3-6}
  @argument[fontmap]{a @class{pango:font-map} object}
  @begin{short}
    Forces a change in the context, which will cause any @class{pango:context}
    object using this font map to change.
  @end{short}
  This function is only useful when implementing a new backend for Pango,
  something applications will not do. Backends should call this function if
  they have attached extra data to the context and such data is changed.
  @see-class{pango:font-map}
  @see-class{pango:context}"
  (fontmap (g:object font-map)))

(export 'font-map-changed)

;;; ----------------------------------------------------------------------------
;;; pango_fontset_get_font
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_fontset_get_font" fontset-font) (g:object font)
 #+liber-documentation
 "@version{2024-3-4}
  @argument[fontset]{a @class{pango:fontset} object}
  @argument[char]{a character or a char code with a Unicode character}
  @return{The @class{pango:font} object.}
  @begin{short}
    Returns the font in the fontset that contains the best glyph for the
    Unicode character @arg{char}.
  @end{short}
  @see-class{pango:fontset}
  @see-class{pango:font}"
  (fontset (g:object fontset))
  (char g:unichar))

(export 'fontset-font)

;;; ----------------------------------------------------------------------------
;;; pango_fontset_get_metrics
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_fontset_get_metrics" fontset-metrics)
    (g:boxed font-metrics :return)
 #+liber-documentation
 "@version{2024-3-4}
  @argument[fontset]{a @class{pango:fontset} object}
  @begin{return}
    A @class{pango:font-metrics} instance.
  @end{return}
  @begin{short}
    Get overall metric information for the fonts in the fontset.
  @end{short}
  @see-class{pango:fontset}
  @see-class{pango:font-metrics}"
  (fontset (g:object fontset)))

(export 'fontset-metrics)

;;; ----------------------------------------------------------------------------
;;; PangoFontsetForeachFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback fontset-foreach-func :boolean
    ((fontset (g:object fontset))
     (font (g:object font))
     (data :pointer))
  (restart-case
    (funcall (glib:get-stable-pointer-value data) fontset font)
    (return () :report "Error in PangoFontsetForeach function." nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'fontset-foreach-func)
      "Callback"
      (liber:symbol-documentation 'fontset-foreach-func)
 "@version{2024-5-25}
  @syntax{lambda (fontset font) => result}
  @argument[fontset]{a @class{pango:fontset} object}
  @argument[font]{a @class{pango:font} object from @arg{fontset}}
  @argument[result]{if @em{true}, stop iteration and return immediately}
  @begin{short}
    A callback function used by the @fun{pango:fontset-foreach} function when
    enumerating the fonts in a fontset.
  @end{short}
  @see-class{pango:fontset}
  @see-function{pango:fontset-foreach}")

(export 'fontset-foreach-func)

;;; ----------------------------------------------------------------------------
;;; pango_fontset_foreach
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_fontset_foreach" %fontset-foreach) :void
  (fontset (g:object fontset))
  (func :pointer)
  (data :pointer))

(defun fontset-foreach (fontset func)
 #+liber-documentation
 "@version{2024-5-25}
  @argument[fontset]{a @class{pango:fontset} object}
  @argument[func]{a @symbol{pango:fontset-foreach-func} callback function}
  @begin{short}
    Iterates through all the fonts in a fontset, calling @arg{func} for each
    one.
  @end{short}
  If @arg{func} returns @em{true}, that stops the iteration.
  @begin[Examples]{dictionary}
    Code fragment for counting the fonts in a fontset and for printing a list
    of the font descriptions.
    @begin{pre}
(let ((count 0))
  (pango:fontset-foreach fontset
          (lambda (fontset font)
            (declare (ignore fontset))
            (let* ((desc (pango:font-describe font))
                   (str (pango:font-description-to-string desc)))
              (format t \"~a : ~a~%\" count str)
              (incf count)
              nil)))
  count)
    @end{pre}
  @end{dictionary}
  @see-class{pango:fontset}
  @see-symbol{pango:fontset-foreach-func}"
  (glib:with-stable-pointer (ptr func)
    (%fontset-foreach fontset
                      (cffi:callback fontset-foreach-func)
                      ptr)))

(export 'fontset-foreach)

;;; --- End of file pango.font.lisp --------------------------------------------
