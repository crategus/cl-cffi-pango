;;; ----------------------------------------------------------------------------
;;; pango.cairo-rendering.lisp
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
;;; Cairo Rendering
;;;
;;;     Font handling and rendering with Cairo
;;;
;;; Types and Values
;;;
;;;     PangoCairoFont
;;;     PangoCairoFontMap
;;;
;;; Functions
;;;
;;;     pango_cairo_font_map_get_default
;;;     pango_cairo_font_map_set_default
;;;     pango_cairo_font_map_new
;;;     pango_cairo_font_map_new_for_font_type
;;;     pango_cairo_font_map_get_font_type
;;;     pango_cairo_font_map_set_resolution
;;;     pango_cairo_font_map_get_resolution
;;;     pango_cairo_font_map_create_context                not exported
;;;     pango_cairo_font_get_scaled_font                   not exported
;;;
;;;     pango_cairo_context_set_resolution
;;;     pango_cairo_context_get_resolution
;;;     pango_cairo_context_set_font_options
;;;     pango_cairo_context_get_font_options
;;;
;;;     PangoCairoShapeRendererFunc
;;;
;;;     pango_cairo_context_set_shape_renderer
;;;     pango_cairo_context_get_shape_renderer
;;;     pango_cairo_create_context
;;;     pango_cairo_update_context
;;;     pango_cairo_create_layout
;;;     pango_cairo_update_layout
;;;     pango_cairo_show_glyph_string
;;;     pango_cairo_show_glyph_item
;;;     pango_cairo_show_layout_line
;;;     pango_cairo_show_layout
;;;     pango_cairo_show_error_underline
;;;     pango_cairo_glyph_string_path
;;;     pango_cairo_layout_line_path
;;;     pango_cairo_layout_path
;;;     pango_cairo_error_underline_path
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ├── PangoCairoFont
;;;     ╰── PangoCairoFontMap
;;;
;;; Prerequisites
;;;
;;;     PangoCairoFont requires PangoFont.
;;;     PangoCairoFontMap requires PangoFontMap.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoCairoFont
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "PangoCairoFont" cairo-font
  (:export t
   :type-initializer "pango_cairo_font_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'cairo-font)
      "Interface"
      (documentation 'cairo-font 'type)
 "@version{2025-08-24}
  @begin{short}
    The @sym{pango:cairo-font} interface is an interface exported by fonts for
    use with Cairo.
  @end{short}
  The actual type of the font will depend on the particular font technology
  Cairo was compiled to use.
  @see-class{pango:cairo-font-map}")

;;; ----------------------------------------------------------------------------
;;; PangoCairoFontMap
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "PangoCairoFontMap" cairo-font-map
  (:export t
   :type-initializer "pango_cairo_font_map_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'cairo-font-map)
      "Interface"
      (documentation 'cairo-font-map 'type)
 "@version{2025-08-24}
  @begin{short}
    The @sym{pango:cairo-font-map} interface is an interface exported by font
    maps for use with Cairo.
  @end{short}
  The actual type of the font map will depend on the particular font technology
  Cairo was compiled to use.
  @see-constructor{pango:cairo-font-map-new}
  @see-constructor{pango:cairo-font-map-new-for-font-type}
  @see-class{pango:cairo-font}")

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_get_default
;;; pango_cairo_font_map_set_default
;;; ----------------------------------------------------------------------------

(defun (setf cairo-font-map-default) (fontmap)
  (cffi:foreign-funcall "pango_cairo_font_map_set_default"
                        (g:object font-map) fontmap
                        :void)
  fontmap)

(cffi:defcfun ("pango_cairo_font_map_get_default" cairo-font-map-default)
    (g:object font-map)
 #+liber-documentation
 "@version{2025-08-24}
  @syntax{(pango:cairo-font-map-default) => fontmap}
  @syntax{(setf (pango:cairo-font-map-default) fontmap)}
  @argument[fontmap]{a @class{pango:font-map} object, or @code{nil}}
  @begin{short}
    Gets or sets a default PangoCairo font map to use with Cairo.
  @end{short}
  Note that the type of the returned object will depend on the particular font
  backend Cairo was compiled to use. You generally should only use the
  @class{pango:font-map} and @class{pango:cairo-font-map} interfaces on the
  returned object.

  This can be used to change the Cairo font backend that the default font map
  uses. The old default font map is unreffed and the new font map referenced.

  Note that the default font map is per-thread. This function only changes the
  default fontmap for the current thread. Default fontmaps of exisiting threads
  are not changed. Default fontmaps of any new threads will still be created
  using the @fun{pango:cairo-font-map-new} function.

  A @code{nil} value for @arg{fontmap} will cause the current default font map
  to be released and a new default font map to be created on demand, using the
  @fun{pango:cairo-font-map-new} function.
  @see-class{pango:font-map}
  @see-class{pango:cairo-font-map}
  @see-function{pango:cairo-font-map-new}")

(export 'cairo-font-map-default)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_font_map_new" cairo-font-map-new)
    (g:object font-map :return)
#+liber-documentation
 "@version{2025-01-01}
  @begin{return}
    The newly allocated @class{pango:font-map} object.
  @end{return}
  @begin{short}
    Creates a new PangoCairo font map object.
  @end{short}
  A font map is used to cache information about available fonts, and holds
  certain global parameters such as the resolution. In most cases, you can use
  the @fun{pango:cairo-font-map-default} function instead.

  Note that the type of the returned object will depend on the particular font
  backend Cairo was compiled to use. You generally should only use the
  @class{pango:font-map} and @class{pango:cairo-font-map} interfaces on the
  returned object.
  @see-class{pango:font-map}
  @see-class{pango:cairo-font-map}
  @see-function{pango:cairo-font-map-default}")

(export 'cairo-font-map-new)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_new_for_font_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_font_map_new_for_font_type"
                cairo-font-map-new-for-font-type) (g:object font-map :return)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[fonttype]{a @sym{cairo:font-type-t} value for the desired font type}
  @begin{return}
    The newly allocated @class{pango:font-map} object of suitable type, or
    @code{nil} if the requested Cairo font backend is not supported or compiled
    in.
  @end{return}
  @begin{short}
    Creates a new @class{pango:font-map} object of the type suitable to be used
    with the Cairo font backend of type @arg{fonttype}.
  @end{short}

  In most cases one should simply use the @fun{pango:cairo-font-map-new}
  function, or in fact in most of those cases, just use the
  @fun{pango:cairo-font-map-default} function.
  @see-class{pango:font-map}
  @see-function{pango:cairo-font-map-new}
  @see-function{pango:cairo-font-map-default}"
  (fonttype cairo:font-type-t))

(export 'cairo-font-map-new-for-font-type)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_get_font_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_font_map_get_font_type" %cairo-font-map-font-type)
    cairo:font-type-t
  (fontmap :pointer))

(defun cairo-font-map-font-type (fontmap)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[fontmap]{a @class{pango:font-map} object}
  @begin{return}
    The @sym{cairo:font-type-t} value for the Cairo font backend type.
  @end{return}
  @begin{short}
    Gets the type of the Cairo font backend that @arg{fontmap} uses.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(pango:cairo-font-map-font-type (pango:cairo-font-map-default))
=> :FT
    @end{pre}
  @end{dictionary}
  @see-class{pango:font-map}
  @see-symbol{cairo:font-type-t}"
  (%cairo-font-map-font-type (if (cffi:pointerp fontmap)
                                 fontmap
                                 (g:object-pointer fontmap))))

(export 'cairo-font-map-font-type)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_get_resolution
;;; pango_cairo_font_map_set_resolution
;;; ----------------------------------------------------------------------------

(defun (setf cairo-font-map-resolution) (dpi fontmap)
  (let ((dpi (coerce dpi 'double-float)))
    (cffi:foreign-funcall "pango_cairo_font_map_set_resolution"
                          :pointer (if (cffi:pointerp fontmap)
                                       fontmap
                                       (g:object-pointer fontmap))
                          :double dpi
                          :void)
    dpi))

(cffi:defcfun ("pango_cairo_font_map_get_resolution" %cairo-font-map-resolution)
    :double
  (fontmap :pointer))

(defun cairo-font-map-resolution (fontmap)
 #+liber-documentation
 "@version{2025-08-24}
  @syntax{(pango:cairo-font-map-resolution fontmap) => dpi}
  @syntax{(setf (pango:cairo-font-map-resolution fontmap) dpi)}
  @argument[fontmap]{a @class{pango:cairo-font-map} object}
  @argument[dpi]{a number coerced to a double float for the resolution in dots
    per inch, physical inches are not actually involved, the terminology is
    conventional}
  @begin{short}
    Gets or sets the resolution for the font map.
  @end{short}
  This is a scale factor between points specified in a
  @class{pango:font-description} instance and Cairo units. The default value is
  96, meaning that a 10 point font will be 13 units high, that is
  @code{(10 * 96 / 72 = 13.3)}.
  @begin[Examples]{dictionary}
    @begin{pre}
(pango:cairo-font-map-resolution (pango:cairo-font-map-default))
=> 96.0d0
    @end{pre}
  @end{dictionary}
  @see-class{pango:cairo-font-map}
  @see-class{pango:font-description}"
  (%cairo-font-map-resolution (if (cffi:pointerp fontmap)
                                  fontmap
                                  (g:object-pointer fontmap))))

(export 'cairo-font-map-resolution)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_map_create_context                     not exported
;;; ----------------------------------------------------------------------------

;; This function is deprectaed and not exported

(cffi:defcfun ("pango_cairo_font_map_create_context"
                cairo-font-map-create-context) (g:object context)
 #+liber-documentation
 "@version{#2025-1-1}
  @argument[fontmap]{a @class{pango:cairo-font-map} object}
  @return{The newly created @class{pango:context} object.}
  @begin{short}
    Create a Pango context for the given @arg{fontmap}.
  @end{short}
  @begin[Warning]{dictionary}
    The @sym{pango:cairo-font-map-create-context} function has been deprecated
    since version 1.22 and should not be used in newly written code. Use the
    @fun{pango:font-map-create-context} function instead.
  @end{dictionary}
  @see-class{pango:cairo-font-map}
  @see-class{pango:context}
  @see-function{pango:font-map-create-context}"
  (fontmap (g:object cairo-font-map)))

;;; ----------------------------------------------------------------------------
;;; pango_cairo_font_get_scaled_font                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_font_get_scaled_font" cairo-font-scaled-font)
    (:pointer (:struct cairo:scaled-font-t))
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[font]{a @class{pango:font} object from a
    @class{pango:cairo-font-map} object}
  @begin{return}
    The @sym{cairo:scaled-font-t} instance used by @arg{font}, or @code{nil} if
    @arg{font} is @code{nil}.
  @end{return}
  @begin{short}
    Gets the @sym{cairo:scaled-font-t} instance used by @arg{font}.
  @end{short}
  The scaled font can be referenced and kept using the
  @fun{cairo-scaled-font-reference} function.
  @see-class{pango:cairo-font}
  @see-symbol{cairo:scaled-font-t}
  @see-function{cairo:scaled-font-reference}"
  (font (g:object cairo-font)))

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_get_resolution
;;; pango_cairo_context_set_resolution
;;; ----------------------------------------------------------------------------

(defun (setf cairo-context-resolution) (dpi context)
  (let ((dpi (coerce dpi 'double-float)))
    (cffi:foreign-funcall "pango_cairo_context_set_resolution"
                          (g:object context) context
                          :double dpi
                          :void)
    dpi))

(cffi:defcfun ("pango_cairo_context_get_resolution" cairo-context-resolution)
    :double
 #+liber-documentation
 "@version{2025-08-24}
  @syntax{(pango:cairo-context-resolution context) => dpi}
  @syntax{(setf (pango:cairo-context-resolution context) dpi)}
  @argument[context]{a @class{pango:context} object, from a PangoCairo font map}
  @argument[dpi]{a number coerced to a double float for the resolution in dots
    per inch, physical inches are not actually involved, the terminology is
    conventional, a 0 or negative value means to use the resolution from the
    font map}
  @begin{short}
    Gets or sets the resolution for the Pango context.
  @end{short}
  This is a scale factor between points specified in a
  @class{pango:font-description} instance and Cairo units. The default value is
  96, meaning that a 10 point font will be 13 units high, that is
  @code{(10 * 96 / 72 = 13.3)}.
  @see-class{pango:context}
  @see-class{pango:font-description}"
  (context (g:object context)))

(export 'cairo-context-resolution)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_get_font_options
;;; pango_cairo_context_set_font_options
;;; ----------------------------------------------------------------------------

(defun (setf cairo-context-font-options) (options context)
  (cffi:foreign-funcall "pango_cairo_context_set_font_options"
                        (g:object context) context
                        (:pointer (:struct cairo:font-options-t))
                          (or options (cffi:null-pointer))
                        :void)
  options)

(cffi:defcfun ("pango_cairo_context_get_font_options"
                %cairo-context-font-options)
    (:pointer (:struct cairo:font-options-t))
  (context (g:object context)))

(defun cairo-context-font-options (context)
 #+liber-documentation
 "@version{2025-08-24}
  @syntax{(pango:cairo-context-font-options context) => options}
  @syntax{(setf (pango:cairo-context-font-options context) options)}
  @argument[context]{a @class{pango:context} object, from a PangoCairo font map}
  @argument[options]{a @sym{cairo:font-options-t} instance, or @code{nil} to
    unset any previously set font options}
  @begin{short}
    Gets or sets the font options used when rendering text with the Pango
    context.
  @end{short}
  This function does not report options that are derived from the target surface
  by the @fun{pango:cairo-update-context} function. These font options override
  any font options that the @fun{pango:cairo-update-context} function derives
  from the target surface.
  @see-class{pango:context}
  @see-symbol{cairo:font-options-t}
  @see-function{pango:cairo-update-context}"
  (let ((options (%cairo-context-font-options context)))
    (unless (cffi:null-pointer-p options)
      options)))

(export 'cairo-context-font-options)

;;; ----------------------------------------------------------------------------
;;; PangoCairoShapeRendererFunc                             not exported
;;; ----------------------------------------------------------------------------

;; TODO: The PANGO:ATTR-SHAPE type is not implemented. Should we remove the
;; implementation of CairoShapeRenderer?

(cffi:defcallback cairo-shape-renderer-func :void
    ((cr (:pointer (:struct cairo:context-t)))
     (attr :pointer)
     (dopath :boolean)
     (data :pointer))
  (restart-case
    (funcall (glib:get-stable-pointer-value data) cr attr dopath)
    (return-from-callback () nil)))

#+liber-documentation
(setf (liber:alias-for-symbol 'cairo-shape-renderer-func)
      "Callback"
      (liber:symbol-documentation 'cairo-shape-renderer-func)
 "@version{#2025-08-24}
  @begin{short}
    The callback function for rendering attributes of @sym{pango:attr-shape}
    type with the Cairo renderer of Pango.
  @end{short}
  @begin{pre}
lambda (cr attr dopath)
  @end{pre}
  @begin[code]{simple-table}
    @entry[cr]{The @sym{cairo:context-t} instance for current point set to
      where the shape should be rendered.}
    @entry[attr]{The @sym{pango:attr-shape} instance for the attributes to
      render.}
    @entry[dopath]{Whether only the shape path should be appended to current
      path of @arg{cr} and no filling/stroking done. This will be set to
      @em{true} when called from the @fun{pango:cairo-layout-path} and the
      @fun{pango:cairo-layout-line-path} rendering functions.}
  @end{simple-table}
  @see-symbol{cairo:context-t}
  @see-symbol{pango:attr-shape}
  @see-function{pango:cairo-layout-path}
  @see-function{pango:cairo-layout-line-path}")

;;; ----------------------------------------------------------------------------
;;; pango_cairo_context_get_shape_renderer                  not exported
;;; pango_cairo_context_set_shape_renderer
;;; ----------------------------------------------------------------------------

(defun (setf cairo-context-shape-renderer) (func context)
  (let ((func (or func (cffi:null-pointer))))
    (cffi:foreign-funcall "pango_cairo_context_set_shape_renderer"
                          (g:object context) context
                          :pointer (cffi:callback cairo-shape-renderer-func)
                          :pointer (glib:allocate-stable-pointer func)
                          :pointer
                          (cffi:callback glib:stable-pointer-destroy-notify)
                          :void)
    func))

(cffi:defcfun ("pango_cairo_context_get_shape_renderer"
                %cairo-context-shape-renderer) :pointer
  (context (g:object context))
  (data :pointer))

(defun cairo-context-shape-renderer (context)
 #+liber-documentation
 "@version{#2025-08-24}
  @syntax{(pango:cairo-context-shape-renderer context) func}
  @syntax{(setf (pango:cairo-context-shape-renderer context) func)}
  @argument[context]{a @class{pango:context} object, from a PangoCairo font map}
  @argument[func]{a @sym{pango:cairo-shape-renderer-func} callback function for
    rendering attributes of @sym{pango:attr-shape} type, or @code{nil} to
    disable shape rendering}
  @begin{short}
    Gets or sets the callback function for rendering attributes of the
    @sym{pango:attr-shape} type.
  @end{short}
  @see-class{pango:context}
  @see-symbol{pango:attr-shape}
  @see-symbol{pango:cairo-shape-renderer-func}"
  (%cairo-context-shape-renderer context (cffi:null-pointer)))

;;; ----------------------------------------------------------------------------
;;; pango_cairo_create_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_create_context" cairo-create-context)
    (g:object context :return)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @return{The newly created @class{pango:context} object.}
  @begin{short}
    Creates a Pango context set up to match the current transformation and
    target surface of the Cairo context.
  @end{short}
  This Pango context can then be used to create a Pango layout using the
  @fun{pango:layout-new} function.

  This function is a convenience function that creates a Pango context using the
  default font map, then updates it to the Cairo context. If you just need to
  create a Pango layout for use with the Cairo context and do not need to access
  the @class{pango:context} object directly, you can use the
  @fun{pango:cairo-create-layout} function instead.
  @see-class{pango:context}
  @see-symbol{cairo:context-t}
  @see-function{pango:layout-new}
  @see-function{pango:cairo-create-layout}"
  (cr (:pointer (:struct cairo:context-t))))

(export 'cairo-create-context)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_update_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_update_context" cairo-update-context) :void
 #+liber-documentation
 "@version{2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[context]{a @class{pango:context} object, from a PangoCairo font map}
  @begin{short}
    Updates a Pango context previously created for use with Cairo to match the
    current transformation and target surface of a Cairo context.
  @end{short}
  If any Pango layouts have been created for the Pango context, it is necessary
  to call the @fun{pango:layout-context-changed} function on those layouts.
  @see-class{pango:context}
  @see-symbol{cairo:context-t}
  @see-function{pango:layout-context-changed}"
  (cr (:pointer (:struct cairo:context-t)))
  (context (g:object context)))

(export 'cairo-update-context)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_create_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_create_layout" cairo-create-layout)
    (g:object layout :return)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @return{The newly created @class{pango:layout} object.}
  @begin{short}
    Creates a Pango layout set up to match the current transformation and
    target surface of the Cairo context.
  @end{short}
  This Pango layout can then be used for text measurement with functions like
  the @fun{pango:layout-size} function or drawing with functions like the
  @fun{pango:cairo-show-layout} function. If you change the transformation or
  target surface for the Cairo context, you need to call the
  @fun{pango:cairo-update-layout} function.

  This function is the most convenient way to use Cairo with Pango, however it
  is slightly inefficient since it creates a separate @class{pango:context}
  object for each Pango layout. This might matter in an application that was
  laying out large amounts of text.
  @see-symbol{cairo:context-t}
  @see-class{pango:layout}
  @see-class{pango:context}
  @see-function{pango:layout-size}
  @see-function{pango:cairo-show-layout}
  @see-function{pango:cairo-update-layout}"
  (cr (:pointer (:struct cairo:context-t))))

(export 'cairo-create-layout)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_update_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_update_layout" cairo-update-layout) :void
 #+liber-documentation
 "@version{2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[layout]{a @class{pango:layout} object}
  @begin{short}
    Updates the @class{pango:context} object of a @class{pango:layout} object
    created with the @fun{pango:cairo-create-layout} function to match the
    current transformation and target surface of a Cairo context.
  @end{short}
  @see-symbol{cairo:context-t}
  @see-class{pango:layout}
  @see-class{pango:context}
  @see-function{pango:cairo-create-layout}"
  (cr (:pointer (:struct cairo:context-t)))
  (layout (g:object layout)))

(export 'cairo-update-layout)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_glyph_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_show_glyph_string" cairo-show-glyph-string) :void
 #+liber-documentation
 "@version{2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[font]{a @class{pango:font} object from a PangoCairo font map}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @begin{short}
    Draws the glyphs in @arg{glyphs} in the specified Cairo context.
  @end{short}
  The origin of the glyphs, the left edge of the baseline, will be drawn at the
  current point of the Cairo context.
  @see-symbol{cairo:context-t}
  @see-class{pango:font}
  @see-class{pango:glyph-string}"
  (cr (:pointer (:struct cairo:context-t)))
  (font (g:object font))
  (glyphs (g:boxed glyph-string)))

(export 'cairo-show-glyph-string)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_glyph_item
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_show_glyph_item" cairo-show-glyph-item) :void
 #+liber-documentation
 "@version{2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[text]{a string for the UTF-8 text that @arg{item} refers to}
  @argument[item]{a @class{pango:glyph-item} instance}
  @begin{short}
    Draws the glyphs in @arg{item} in the specified Cairo context, embedding
    the text associated with the glyphs in the output if the output format
    supports it, for example PDF, otherwise it acts similar to the
    @fun{pango:cairo-show-glyph-string} function.
  @end{short}

  The origin of the glyphs, the left edge of the baseline, will be drawn at
  the current point of the Cairo context.
  @see-symbol{cairo:context-t}
  @see-class{pango:glyph-item}
  @see-function{pango:cairo-show-glyph-string}"
  (cr (:pointer (:struct cairo:context-t)))
  (text :string)
  (item (g:boxed glyph-item)))

(export 'cairo-show-glyph-item)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_layout_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_show_layout_line" cairo-show-layout-line) :void
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[line]{a @class{pango:layout-line} instance}
  @begin{short}
    Draws a Pango layout line in the specified Cairo context.
  @end{short}
  The origin of the glyphs, the left edge of the line, will be drawn at the
  current point of the Cairo context.
  @see-symbol{cairo:context-t}
  @see-class{pango:layout-line}"
  (cr (:pointer (:struct cairo:context-t)))
  (line (g:boxed layout-line)))

(export 'cairo-show-layout-line)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_show_layout" cairo-show-layout) :void
 #+liber-documentation
 "@version{2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[layout]{a @class{pango:layout} object}
  @begin{short}
    Draws a Pango layout in the specified Cairo context.
  @end{short}
  The top-left corner of the @class{pango:layout} object will be drawn at the
  current point of the Cairo context.
  @see-symbol{cairo:context-t}
  @see-class{pango:layout}"
  (cr (:pointer (:struct cairo:context-t)))
  (layout (g:object layout)))

(export 'cairo-show-layout)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_show_error_underline
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_show_error_underline" %cairo-show-error-underline)
    :void
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun cairo-show-error-underline (cr x y width height)
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[x]{a number for the x coordinate of one corner of the rectangle}
  @argument[y]{a number for the y coordinate of one corner of the rectangle}
  @argument[width]{a number for the width of the rectangle}
  @argument[height]{a number for the height of the rectangle}
  @begin{short}
    Draw a squiggly line in the specified Cairo context that approximately
    covers the given rectangle in the style of an underline used to indicate
    a spelling error.
  @end{short}
  The width of the underline is rounded to an integer number of up and down
  segments and the resulting rectangle is centered in the original rectangle.
  @begin[Notes]{dictionary}
    All numbers are coerced to a double float before being passed to the
    foreign C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}"
  (%cairo-show-error-underline cr (coerce x 'double-float)
                                  (coerce y 'double-float)
                                  (coerce width 'double-float)
                                  (coerce height 'double-float)))

(export 'cairo-show-error-underline)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_glyph_string_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_glyph_string_path" cairo-glyph-string-path) :void
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[font]{a @class{pango:font} object from a PangoCairo font map}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @begin{short}
    Adds the glyphs in @arg{glyphs} to the current path in the specified Cairo
    context.
  @end{short}
  The origin of the glyphs, the left edge of the baseline, will be at the
  current point of the Cairo context.
  @see-symbol{cairo:context-t}
  @see-class{pango:font}
  @see-class{pango:glyph-string}"
  (cr (:pointer (:struct cairo:context-t)))
  (font (g:object font))
  (glyphs (g:boxed glyph-string)))

(export 'cairo-glyph-string-path)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_layout_line_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_layout_line_path" cairo-layout-line-path) :void
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[line]{a @class{pango:layout-line} instance}
  @begin{short}
    Adds the text in the @class{pango:layout-line} instance to the current path
    in the specified Cairo context.
  @end{short}
  The origin of the glyphs, the left edge of the line, will be at the current
  point of the Cairo context.
  @see-symbol{cairo:context-t}
  @see-class{pango:layout-line}"
  (cr (:pointer (:struct cairo:context-t)))
  (line (g:object layout-line)))

(export 'cairo-layout-line-path)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_layout_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_layout_path" cairo-layout-path) :void
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[layout]{a @class{pango:layout} object}
  @begin{short}
    Adds the text in a Pango layout to the current path in the specified Cairo
    context.
  @end{short}
  The top-left corner of the Pango layout will be at the current point of the
  Cairo context.
  @see-symbol{cairo:context-t}
  @see-class{pango:layout}"
  (cr (:pointer (:struct cairo:context-t)))
  (layout (g:object layout)))

(export 'cairo-layout-path)

;;; ----------------------------------------------------------------------------
;;; pango_cairo_error_underline_path
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_cairo_error_underline_path" %cairo-error-underline-path)
    :void
  (cr (:pointer (:struct cairo:context-t)))
  (x :double)
  (y :double)
  (width :double)
  (height :double))

(defun cairo-error-underline-path (cr x y width height)
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[cr]{a @sym{cairo:context-t} instance}
  @argument[x]{a number for the x coordinate of one corner of the rectangle}
  @argument[y]{a number for the y coordinate of one corner of the rectangle}
  @argument[width]{a number for the width of the rectangle}
  @argument[height]{a number for the height of the rectangle}
  @begin{short}
    Add a squiggly line to the current path in the specified Cairo context that
    approximately covers the given rectangle in the style of an underline used
    to indicate a spelling error.
  @end{short}
  The width of the underline is rounded to an integer number of up and down
  segments and the resulting rectangle is centered in the original rectangle.
  @begin[Notes]{dictionary}
    All numbers are coerced to a double float before being passed to the
    foreign C function.
  @end{dictionary}
  @see-symbol{cairo:context-t}"
  (%cairo-error-underline-path cr (coerce x 'double-float)
                                  (coerce y 'double-float)
                                  (coerce width 'double-float)
                                  (coerce height 'double-float)))

(export 'cairo-error-underline-path)

;;; --- End of file pango.cairo-rendering.lisp ---------------------------------
