;;; ----------------------------------------------------------------------------
;;; pango.context.lisp
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
;;; Contexts
;;;
;;;     Global context object
;;;
;;; Types and Values
;;;
;;;     PangoContext
;;;
;;; Functions
;;;
;;;     pango_context_new
;;;     pango_context_changed
;;;     pango_context_get_serial
;;;     pango_context_set_font_map
;;;     pango_context_get_font_map
;;;     pango_context_get_font_description
;;;     pango_context_set_font_description
;;;     pango_context_get_language
;;;     pango_context_set_language
;;;     pango_context_get_base_dir
;;;     pango_context_set_base_dir
;;;     pango_context_get_base_gravity
;;;     pango_context_set_base_gravity
;;;     pango_context_get_gravity
;;;     pango_context_get_gravity_hint
;;;     pango_context_set_gravity_hint
;;;     pango_context_get_matrix
;;;     pango_context_set_matrix
;;;     pango_context_get_round_glyph_positions
;;;     pango_context_set_round_glyph_positions
;;;     pango_context_load_font
;;;     pango_context_load_fontset
;;;     pango_context_get_metrics
;;;     pango_context_list_families
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── PangoContext
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoContext
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "PangoContext" context
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_context_get_type")
  nil)

#+liber-documentation
(setf (documentation 'context 'type)
 "@version{2024-2-23}
  @begin{short}
    The @symbol{pango:context} object stores global information influencing
    the operation of Pango, such as the font map used to look up fonts, and
    default values such as the default language, default gravity, or default
    font.
  @end{short}
  @see-constructor{pango:context-new}")

;;; ----------------------------------------------------------------------------
;;; pango_context_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline context-new))

(defun context-new ()
 #+liber-documentation
 "@version{2024-2-23}
  @return{The newly allocated @class{pango:context} object.}
  @begin{short}
    Creates a new Pango Context initialized to default values.
  @end{short}
  This function is not particularly useful as it should always be followed by
  a call of the @fun{pango:context-font-map} function, and the
  @fun{pango:font-map-create-context} function does these two steps together
  and hence users are recommended to use that.

  If you are using Pango as part of a higher-level system, that system may
  have its own way of create a Pango context. For instance, the GTK toolkit
  has, among others, the @fun{gtk:widget-pango-context} function. Use those
  instead.
  @see-class{pango:context}
  @see-function{pango:context-font-map}
  @see-function{pango:font-map-create-context}
  @see-function{gtk:widget-pango-context}"
  (make-instance 'context))

(export 'context-new)

;;; ----------------------------------------------------------------------------
;;; pango_context_changed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_context_changed" context-changed) :void
 #+liber-documentation
 "@version{2024-2-23}
  @argument[context]{a @class{pango:context} object}
  @begin{short}
    Forces a change in the Pango context, which will cause any
    @class{pango:layout} object using this Pango context to re-layout.
  @end{short}
  This function is only useful when implementing a new backend for Pango,
  something applications will not do. Backends should call this function if
  they have attached extra data to the context and such data is changed.
  @see-class{pango:context}
  @see-class{pango:layout}"
  (context (g:object context)))

(export 'context-changed)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_serial ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_context_get_serial" context-serial) :uint
 #+liber-documentation
 "@version{2024-2-23}
  @argument[context]{a @class{pango:context} object}
  @return{An unsigned integer with the current serial number of @arg{context}.}
  @begin{short}
    Returns the current serial number of the Pango context.
  @end{short}
  The serial number is initialized to a small number larger than zero when a
  new Pango context is created and is increased whenever the Pango context is
  changed using any of the setter functions, or the @class{pango:font-map}
  object it uses to find fonts has changed. The serial may wrap, but will never
  have the value 0. Since it can wrap, never compare it with \"less than\",
  always use \"not equals\".

  This can be used to automatically detect changes to a @class{pango:context}
  object, and is only useful when implementing objects that need update when
  their Pango context changes, like their Pango layout.
  @see-class{pango:context}
  @see-class{pango:font-map}"
  (context (g:object context)))

(export 'context-serial)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_font_map ()
;;; pango_context_set_font_map ()
;;; ----------------------------------------------------------------------------

(defun (setf context-font-map) (font-map context)
  (cffi:foreign-funcall "pango_context_set_font_map"
                        (g:object context) context
                        (g:object font-map) font-map
                        :void)
  font-map)

(cffi:defcfun ("pango_context_get_font_map" context-font-map)
    (g:object font-map)
 #+liber-documentation
 "@version{2024-2-23}
  @syntax[]{(pango:context-font-map context) => fontmap}
  @syntax[]{(setf (pango:context-font-map context) fontmap)}
  @argument[context]{a @class{pango:context} object}
  @argument[fontmap]{a @class{pango:font-map} object}
  @begin{short}
    The @fun{pango:context-font-map} function gets the @class{pango:font-map}
    object used to look up fonts for this context.
  @end{short}
  The @setf{pango:context-font-map} function sets the font map to be searched
  when fonts are looked-up in this Pango context. This is only for internal use
  by Pango backends, a @class{pango:context} object obtained via one of the
  recommended methods should already have a suitable font map.
  @see-class{pango:context}
  @see-class{pango:font-map}"
  (context (g:object context)))

(export 'context-font-map)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_font_description ()
;;; pango_context_set_font_description ()
;;; ----------------------------------------------------------------------------

(defun (setf context-font-description) (desc context)
  (cffi:foreign-funcall "pango_context_set_font_description"
                        (g:object context) context
                        (g:boxed font-description) desc
                        :void)
  desc)

(cffi:defcfun ("pango_context_get_font_description" context-font-description)
    (g:boxed font-description)
 #+liber-documentation
 "@version{2024-2-23}
  @syntax[]{(pango:context-font-description context) => desc}
  @syntax[]{(setf (pango:context-font-description context) desc)}
  @argument[context]{a @class{pango:context} object}
  @argument[desc]{a @class{pango:font-description} instance}
  @begin{short}
    The @symbol{pango:context-font-description} function retrieves the default
    font description for the Pango context.
  @end{short}
  The @setf{pango:context-font-description} function sets the default font
  description for the Pango context.
  @see-class{pango:context}
  @see-class{pango:font-description}"
  (context (g:object context)))

(export 'context-font-description)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_language ()
;;; pango_context_set_language ()
;;; ----------------------------------------------------------------------------

(defun (setf context-language) (language context)
  (cffi:foreign-funcall "pango_context_set_language"
                        (g:object context) context
                        (g:boxed language) language)
  language)

(cffi:defcfun ("pango_context_get_language" context-language) (g:boxed language)
 #+liber-documentation
 "@version{2024-2-23}
  @syntax[]{(pango:context-language context) => language}
  @syntax[]{(setf (pango:context-language context) language)}
  @argument[context]{a @class{pango:context} object}
  @argument[language]{a @class{pango:language} instance, or @code{nil}}
  @begin{short}
    The @symbol{pango:context-language} function retrieves the global language
    tag for the Pango context.
  @end{short}
  The @setf{pango:context-language} function sets the global language. The
  default language for the locale of the running process can be found using the
  @fun{pango:language-default} function.
  @see-class{pango:context}
  @see-class{pango:language}
  @see-function{pango:language-default}"
  (context (g:object context)))

(export 'context-language)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_base_dir ()
;;; pango_context_set_base_dir ()
;;; ----------------------------------------------------------------------------

(defun (setf context-base-dir) (direction context)
  (cffi:foreign-funcall "pango_context_set_base_dir"
                        (g:object context) context
                        direction direction
                        :void)
  direction)

(cffi:defcfun ("pango_context_get_base_dir" context-base-dir) direction
 #+liber-documentation
 "@version{2024-2-23}
  @syntax[]{(pango:context-base-dir context) => direction}
  @syntax[]{(setf (pango:context-base-dir context) direction)}
  @argument[context]{a @class{pango:context}}
  @argument[direction]{a @symbol{pango:direction} value with the base direction}
  @begin{short}
    The @symbol{pango:context-base-dir} function retrieves the base direction
    for the Pango context.
  @end{short}
  The @setf{pango:context-base-dir} function sets the base direction.

  The base direction is used in applying the Unicode bidirectional algorithm.
  If the direction is @code{:ltr} or @code{:rtl}, then the value will be used
  as the paragraph direction in the Unicode bidirectional algorithm. A
  @code{:weak-ltr} value or @code{:weak-rtl} value is used only for paragraphs
  that do not contain any strong characters themselves.
  @see-class{pango:context}
  @see-symbol{pango:direction}"
  (context (g:object context)))

(export 'context-base-dir)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_base_gravity ()
;;; pango_context_set_base_gravity ()
;;; ----------------------------------------------------------------------------

(defun (setf context-base-gravity) (gravity context)
  (cffi:foreign-funcall "pango_context_set_base_gravity"
                        (g:object context) context
                        gravity gravity
                        :void)
  gravity)

(cffi:defcfun ("pango_context_get_base_gravity" context-base-gravity) gravity
 #+liber-documentation
 "@version{2024-2-23}
  @syntax[]{(pango:context-base-gravity context) => gravity}
  @syntax[]{(setf (pango:context-base-gravity context) gravity)}
  @argument[context]{a @class{pango:context} object}
  @argument[gravity]{a @symbol{pango:gravity} value with the base gravity}
  @begin{short}
    The @symbol{pango:context-base-gravity} function retrieves the base gravity
    for the Pango context.
  @end{short}
  The @setf{pango:context-base-gravity} function sets the base gravity.

  The base gravity is used in laying vertical text out.
  @see-class{pango:context}
  @see-symbol{pango:gravity}"
  (context (g:object context)))

(export 'context-base-gravity)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_gravity ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_context_get_gravity" context-gravity) gravity
 #+liber-documentation
 "@version{2024-2-23}
  @argument[context]{a @class{pango:context} object}
  @begin{return}
    The @symbol{pango:gravity} value with the resolved gravity for the Pango
    context.
  @end{return}
  @begin{short}
    Retrieves the gravity for the Pango context.
  @end{short}
  This is similar to the @fun{pango:context-base-gravity} function, except for
  when the base gravity is @code{:auto} for which the
  @fun{pango:gravity-for-matrix} function is used to return the gravity from the
  current Pango context matrix.
  @see-class{pango:context}
  @see-symbol{pango:gravity}
  @see-function{pango:context-base-gravity}
  @see-function{pango:gravity-for-matrix}"
  (context (g:object context)))

(export 'context-gravity)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_gravity_hint ()
;;; pango_context_set_gravity_hint ()
;;; ----------------------------------------------------------------------------

(defun (setf context-gravity-hint) (hint context)
  (cffi:foreign-funcall "pango_context_set_gravity_hint"
                        (g:object context) context
                        gravity-hint hint
                        :void)
  hint)

(cffi:defcfun ("pango_context_get_gravity_hint" context-gravity-hint)
    gravity-hint
 #+liber-documentation
 "@version{#2024-2-23}
  @syntax[]{(pango:context-gravity-hint context) => hint}
  @syntax[]{(setf (pango:context-gravity-hint context) hint)}
  @argument[context]{a @class{pango:context} object}
  @argument[hint]{a @symbol{pango:gravity-hint} value with the gravity hint}
  @begin{short}
    The @fun{pango:context-gravity-hint} function retrieves the gravity hint for
    the Pango context.
  @end{short}
  The @setf{pango:context-gravity-hint} function sets the gravity hint.

  The gravity hint is used in laying vertical text out, and is only relevant
  if gravity of the Pango context as returned by the @fun{pango:context-gravity}
  function is set to the @code{:east} or the @code{:west} value.
  @see-class{pango:context}
  @see-symbol{pango:gravity-hint}"
  (context (g:object context)))

(export 'context-gravity-hint)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_matrix ()
;;; pango_context_set_matrix ()
;;; ----------------------------------------------------------------------------

(defun (setf context-matrix) (matrix context)
  (cffi:foreign-funcall "pango_context_set_matrix"
                        (g:object context) context
                        (g:boxed matrix) matrix
                        :void)
  matrix)

(cffi:defcfun ("pango_context_get_matrix" context-matrix) (g:boxed matrix)
 #+liber-documentation
 "@version{#2024-2-23}
  @syntax[]{(pango:context-matrix context) => matrix}
  @syntax[]{(setf (pango:context-matrix context) matrix)}
  @argument[context]{a @class{pango:context} object}
  @argument[matrix]{a @class{pango:matrix} instance, or @code{nil} to unset any
    existing matrix, no matrix set is the same as setting the identity matrix}
  @begin{short}
    The @symbol{pango:contex-matrix} function gets the transformation matrix
    that will be applied when rendering with this Pango context.
  @end{short}
  The @setf{pango:context-matrix} function sets the transformation matrix that
  will be applied when rendering with this Pango context. Note that reported
  metrics are in the user space coordinates before the application of the
  matrix, not device-space coordinates after the application of the matrix. So,
  they do not scale with the matrix, though they may change slightly for
  different matrices, depending on how the text is fit to the pixel grid.
  @see-class{pango:context}
  @see-class{pango:matrix}"
  (context (g:object context)))

(export 'context-matrix)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_round_glyph_positions ()
;;; pango_context_set_round_glyph_positions ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defun (setf context-round-glyph-positions) (setting context)
  (cffi:foreign-funcall "pango_context_set_round_glyph_positions"
                        (g:object context) context
                        :boolean setting
                        :void)
  setting)

#+pango-1-44
(cffi:defcfun ("pango_context_get_round_glyph_positions"
                context-round-glyph-positions) :boolean
 #+liber-documentation
 "@version{#2024-2-23}
  @syntax[]{(pango:context-round-glyph-positions context) => setting}
  @syntax[]{(setf (pango:context-round-glyph-positions context) setting)}
  @argument[context]{a @class{pango:context} object}
  @argument[setting]{a boolean whether to round glyph positions}
  @begin{short}
    The @fun{pango:context-round-glyph-positions} function returns whether font
    rendering with this Pango context should round glyph positions and widths.
  @end{short}
  The @setf{pango:context-round-glyph-positions} function sets whether
  font rendering with this Pango context should round glyph positions and widths
  to integral positions, in device units.

  This is useful when the renderer cannot handle subpixel positioning of glyphs.
  The default value is to round glyph positions, to remain compatible with
  previous Pango behavior.

  Since 1.44
  @see-class{pango:context}"
  (context (g:object context)))

#+pango-1-44
(export 'context-round-glyph-positions)

;;; ----------------------------------------------------------------------------
;;; pango_context_load_font ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_context_load_font" context-load-font) (g:object font)
 #+liber-documentation
 "@version{2024-2-23}
  @argument[context]{a @class{pango:context} object}
  @argument[desc]{a @class{pango:font-description} instance describing the font
    to load}
  @begin{return}
    The newly allocated @class{pango:font} object that was loaded, or
    @code{nil} if no font matched.
  @end{return}
  @begin{short}
    Loads the font in one of the font maps in the Pango context that is the
    closest match for @arg{desc}.
  @end{short}
  @begin[Example]{dictionary}
    Get the font for a Pango context.
    @begin{pre}
(let* ((fontmap (pango:cairo-font-map-default))
       (context (pango:font-map-create-context fontmap))
       (desc (pango:font-description-from-string \"Sans\"))
       (font (pango:context-load-font context desc)))
  font)
=> #<PANGO:FONT {1002920D73@}>
    @end{pre}
  @end{dictionary}
  @see-class{pango:context}
  @see-class{pango:font-description}
  @see-class{pango:font}"
  (context (g:object context))
  (desc (g:boxed font-description)))

(export 'context-load-font)

;;; ----------------------------------------------------------------------------
;;; pango_context_load_fontset ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_context_load_fontset" context-load-fontset)
    (g:object fontset)
 #+liber-documentation
 "@version{2024-2-23}
  @argument[context]{a @class{pango:context} object}
  @argument[desc]{a @class{pango:font-description} instance describing the
    fonts to load}
  @argument[language]{a @class{pango:language} instance with the fonts will be
    used for}
  @begin{return}
    The newly allocated @class{pango:fontset} object loaded, or @code{nil} if
    no font matched.
  @end{return}
  @begin{short}
    Load a set of fonts in the Pango context that can be used to render a font
    matching @arg{desc}.
  @end{short}
  @begin{dictionary}
    Get the fontset for a Pango context.
    @begin{pre}
(let* ((fontmap (pango:cairo-font-map-default))
       (context (pango:font-map-create-context fontmap))
       (desc (pango:font-description-from-string \"Sans\"))
       (lang (pango:language-default))
       (fontset (pango:context-load-fontset context desc lang)))
  fontset)
=> #<PANGO:FONTSET {10040CFD33@}>
    @end{pre}
  @end{dictionary}
  @see-class{pango:context}
  @see-class{pango:font-description}
  @see-class{pango:language}
  @see-class{pango:fontset}"
  (context (g:object context))
  (desc (g:boxed font-description))
  (language (g:boxed language)))

(export 'context-load-fontset)

;;; ----------------------------------------------------------------------------
;;; pango_context_get_metrics ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_context_get_metrics" context-metrics)
    (g:boxed font-metrics)
 #+liber-documentation
 "@version{2024-2-23}
  @argument[context]{a @class{pango:context} object}
  @argument[desc]{a @class{pango:font-description} instance, @code{nil} means
    that the font description from the context will be used}
  @argument[language]{a @class{pango:language} instance with the language tag of
    used to determine which script to get the metrics for, @code{nil} means that
    the language tag from the context will be used, if no language tag is set on
    the Pango context, metrics for the default language, as determined by the
    @fun{pango:language-default} function, will be returned}
  @return{The @class{pango:font-metrics} instance.}
  @begin{short}
    Get overall metric information for a particular font description.
  @end{short}
  Since the metrics may be substantially different for different scripts, a
  language tag can be provided to indicate that the metrics should be retrieved
  that correspond to the script(s) used by that language.

  The @class{pango:font-description} instance is interpreted in the same way as
  by the @fun{pango:itemize} function, and the family name may be a comma
  separated list of figures. If characters from multiple of these families
  would be used to render the string, then the returned fonts would be a
  composite of the metrics for the fonts loaded for the individual families.
  @see-class{pango:context}
  @see-class{pango:font-description}
  @see-class{pango:language}
  @see-class{pango:font-metrics}
  @see-function{pango:itemize}
  @see-function{pango:language-default}"
  (context (g:object context))
  (desc (g:boxed font-description))
  (language (g:boxed language)))

(export 'context-metrics)

;;; ----------------------------------------------------------------------------
;;; pango_context_list_families ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_context_list_families" %context-list-families) :void
  (context (g:object context))
  (families :pointer)
  (n-families (:pointer :int)))

(defun context-list-families (context)
 #+liber-documentation
 "@version{2024-2-23}
  @argument[context]{a @class{pango:context} object}
  @begin{return}
    A list of @class{pango:font-family} objects.
  @end{return}
  @begin{short}
    List all families for a Pango context.
  @end{short}
  @see-class{pango:context}
  @see-class{pango:font-family}"
  (cffi:with-foreign-objects ((families-ptr :pointer) (n-families :int))
    (%context-list-families context families-ptr n-families)
    (iter (with families-ar = (cffi:mem-ref families-ptr :pointer))
          (for i from 0 below (cffi:mem-ref n-families :int))
          (collect (cffi:convert-from-foreign (cffi:mem-aref families-ar
                                                             :pointer
                                                             i)
                                              'g:object))
          (finally (glib:free families-ar)))))

(export 'context-list-families)

;;; --- End of file pango.context.lisp -----------------------------------------
