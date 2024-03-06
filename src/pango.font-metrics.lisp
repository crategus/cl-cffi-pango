;;; ----------------------------------------------------------------------------
;;; pango.font-metrics.lisp
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
;;;     PangoFontMetrics
;;;
;;; Functions
;;;
;;;     pango_font_metrics_ref                             not needed
;;;     pango_font_metrics_unref                           not needed
;;;     pango_font_metrics_get_ascent
;;;     pango_font_metrics_get_descent
;;;     pango_font_metrics_get_height
;;;     pango_font_metrics_get_approximate_char_width
;;;     pango_font_metrics_get_approximate_digit_width
;;;     pango_font_metrics_get_underline_thickness
;;;     pango_font_metrics_get_underline_position
;;;     pango_font_metrics_get_strikethrough_thickness
;;;     pango_font_metrics_get_strikethrough_position

;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── PangoFontMetrics
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; struct PangoFontMetrics
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque font-metrics "PangoFontMetrics"
  :export t
  :type-initializer "pango_font_metrics_get_type"
  :alloc (error "PangoFontMetrics cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'font-metrics)
      "GBoxed"
      (documentation 'font-metrics 'type)
 "@version{2023-8-27}
  @begin{short}
    A @class{pango:font-metrics} structure holds the overall metric information
    for a font, possibly restricted to a script.
  @end{short}
  The fields of this structure are private to implementations of a font backend.
  See the documentation of the corresponding getters for documentation of their
  meaning.
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

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_ref ()                              not needed
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_font_metrics_ref" font-metrics-ref)
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

#+nil
(export 'font-metrics-ref)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_unref ()                            not needed
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_font_metrics_unref" font-metrics-unref) :void
 #+liber-documentation
 "@version{#2021-1-8}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @begin{short}
    Decrease the reference count of a font metrics instance by one.
  @end{short}
  If the result is zero, frees the instance and any associated memory.
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

#+nil
(export 'font-metrics-unref)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_ascent ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_metrics_get_ascent" font-metrics-ascent) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{The integer with the ascent, in Pango units.}
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
;;; pango_font_metrics_get_descent ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_metrics_get_descent" font-metrics-descent) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{The integer with the descent, in Pango units.}
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
;;; pango_font_metrics_get_height ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(cffi:defcfun ("pango_font_metrics_get_height" font-metrics-height) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{The integer with the height, in Pango units.}
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_metrics_get_approximate_char_width"
                font-metrics-approximate-char-width) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{The integer with the character width, in Pango units.}
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_metrics_get_approximate_digit_width"
                font-metrics-approximate-digit-width) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{The integer with the digit width, in Pango units.}
  @begin{short}
    Gets the approximate digit width for a font metrics instance.
  @end{short}
  This is merely a representative value useful, for example, for determining
  the initial size for a window. Actual digits in text can be wider or narrower
  than this, though this value is generally somewhat more accurate than the
  result of the @fun{pango:font-metrics-approximate-char-width} function for
  digits.
  @see-class{pango:font-metrics}
  @see-function{pango:font-metrics-approximate-char-width}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-approximate-digit-width)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_underline_thickness ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_metrics_get_underline_thickness"
                font-metrics-underline-thickness) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{The integer with the suggested underline thickness, in Pango units.}
  @begin{short}
    Gets the suggested thickness to draw for the underline.
  @end{short}
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-underline-thickness)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_underline_position ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_metrics_get_underline_position"
                font-metrics-underline-position) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @return{The integer with the suggested underline position, in Pango units.}
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_metrics_get_strikethrough_thickness"
                font-metrics-strikethrough-thickness) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @begin{return}
    The integer with the suggested strikethrough thickness, in Pango units.
  @end{return}
  @begin{short}
    Gets the suggested thickness to draw for the strikethrough.
  @end{short}
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-strikethrough-thickness)

;;; ----------------------------------------------------------------------------
;;; pango_font_metrics_get_strikethrough_position ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_font_metrics_get_strikethrough_position"
                font-metrics-strikethrough-position) :int
 #+liber-documentation
 "@version{2024-3-4}
  @argument[metrics]{a @class{pango:font-metrics} instance}
  @begin{return}
    The integer with the suggested strikethrough position, in Pango units.
  @end{return}
  @begin{short}
    Gets the suggested position to draw the strikethrough.
  @end{short}
  The value returned is the distance above the baseline of the top of the
  strikethrough.
  @see-class{pango:font-metrics}"
  (metrics (g:boxed font-metrics)))

(export 'font-metrics-strikethrough-position)

;;; -- End of file pango.font-metrics.lisp -------------------------------------
