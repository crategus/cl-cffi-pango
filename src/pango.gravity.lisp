;;; ----------------------------------------------------------------------------
;;; pango.gravity.lisp
;;;
;;; The documentation in this file is taken from the Pango Reference Manual
;;; Version 1.54 and modified to document the Lisp binding to the Pango
;;; library, see <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2025 Dieter Kaiser
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
;;; Vertical Text
;;;
;;;     Laying text out in vertical directions
;;;
;;; Types and Values
;;;
;;;     PangoGravity
;;;     PangoGravityHint
;;;
;;; Functions
;;;
;;;     PANGO_GRAVITY_IS_IMPROPER
;;;     PANGO_GRAVITY_IS_VERTICAL
;;;     pango_gravity_get_for_matrix
;;;     pango_gravity_get_for_script
;;;     pango_gravity_get_for_script_and_width
;;;     pango_gravity_to_rotation
;;;
;;; Object Hierarchy
;;;
;;;     GEnum
;;;     ├── PangoGravity
;;;     ╰── PangoGravityHint
;;;
;;; Description
;;;
;;; Pango is able to correctly lay vertical text out. In fact, it can set
;;; layouts of mixed vertical and non-vertical text. This section describes the
;;; types used for setting vertical text parameters.
;;;
;;; The way this is implemented is through the concept of gravity. Gravity of
;;; normal Latin text is south. A gravity value of east means that glyphs will
;;; be rotated ninety degrees counterclockwise. So, to render vertical text one
;;; needs to set the gravity and rotate the layout using the matrix machinery
;;; already in place. This has the huge advantage that most algorithms working
;;; on a PangoLayout do not need any change as the assumption that lines run in
;;; the X direction and stack in the Y direction holds even for vertical text
;;; layouts.
;;;
;;; Applications should only need to set base gravity on PangoContext in use,
;;; and let Pango decide the gravity assigned to each run of text. This
;;; automatically handles text with mixed scripts. A very common use is to set
;;; the context base gravity to auto using pango_context_set_base_gravity() and
;;; rotate the layout normally. Pango will make sure that Asian languages take
;;; the right form, while other scripts are rotated normally.
;;;
;;; The correct way to set gravity on a layout is to set it on the context
;;; associated with it using pango_context_set_base_gravity(). The context of a
;;; layout can be accessed using pango_layout_get_context(). The currently set
;;; base gravity of the context can be accessed using
;;; pango_context_get_base_gravity() and the resolved gravity of it using
;;; pango_context_get_gravity(). The resolved gravity is the same as the base
;;; gravity for the most part, except that if the base gravity is set to
;;; PANGO_GRAVITY_AUTO, the resolved gravity will depend on the current matrix
;;; set on context, and is derived using pango_gravity_get_for_matrix().
;;;
;;; The next thing an application may want to set on the context is the gravity
;;; hint. A PangoGravityHint instructs how different scripts should react to
;;; the set base gravity.
;;;
;;; Font descriptions have a gravity property too, that can be set using
;;; pango_font_description_set_gravity() and accessed using
;;; pango_font_description_get_gravity(). However, those are rarely useful from
;;; application code and are mainly used by PangoLayout internally.
;;;
;;; Last but not least, one can create PangoAttributes for gravity and gravity
;;; hint using pango_attr_gravity_new() and pango_attr_gravity_hint_new().
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoGravity
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoGravity" gravity
  (:export t
   :type-initializer "pango_gravity_get_type")
  (:south 0)
  (:east 1)
  (:north 2)
  (:west 3)
  (:auto 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'gravity)
      "GEnum"
      (liber:symbol-documentation 'gravity)
 "@version{2024-2-22}
  @begin{declaration}
(gobject:define-genum \"PangoGravity\" gravity
  (:export t
   :type-initializer \"pango_gravity_get_type\")
  (:south 0)
  (:east 1)
  (:north 2)
  (:west 3)
  (:auto 4))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:south]{Glyphs stand upright (default).}
      @entry[:east]{Glyphs are rotated 90 degrees clockwise.}
      @entry[:north]{Glyphs are upside-down.}
      @entry[:west]{Glyphs are rotated 90 degrees counter-clockwise.}
      @entry[:auto]{Gravity is resolved from the context matrix.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{pango:gravity} enumeration represents the orientation of glyphs
    in a segment of text.
  @end{short}
  This is useful when rendering vertical text layouts. In those situations, the
  layout is rotated using a non-identity @class{pango:matrix} instance, and
  then glyph orientation is controlled using a @symbol{pango:gravity} value.
  Not every value in this enumeration makes sense for every usage. For example,
  the @code{:auto} value only can be passed to and returned by the
  @fun{pango:context-base-gravity} function.
  @see-symbol{pango:gravity-hint}
  @see-class{pango:matrix}
  @see-function{pango:context-base-gravity}")

;;; ----------------------------------------------------------------------------
;;; PangoGravityHint
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoGravityHint" gravity-hint
  (:export t
   :type-initializer "pango_gravity_hint_get_type")
  (:natural 0)
  (:strong 1)
  (:line 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'gravity-hint)
      "GEnum"
      (liber:symbol-documentation 'gravity-hint)
 #+liber-documentation
 "@version{2024-2-22}
  @begin{declaration}
(gobject:define-genum \"PangoGravityHint\" gravity-hint
  (:export t
   :type-initializer \"pango_gravity_hint_get_type\")
  (:natural 0)
  (:strong 1)
  (:line 2))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:natural]{Scripts will take their natural gravity based on the
        base gravity and the script. This is the default.}
      @entry[:strong]{Always use the base gravity set, regardless of the
        script.}
      @entry[:line]{For scripts not in their natural direction, e.g. Latin in
        East gravity, choose per-script gravity such that every script respects
        the line progression. This means, Latin and Arabic will take opposite
        gravities and both flow top-to-bottom for example.}
    @end{table}
  @end{values}
  @begin{short}
    The @symbol{pango:gravity-hint} enumeration defines how horizontal scripts
    should behave in a vertical context.
  @end{short}
  That is, English excerpt in a vertical paragraph for example.
  @see-symbol{pango:gravity}")

;;; ----------------------------------------------------------------------------
;;; PANGO_GRAVITY_IS_IMPROPER()
;;;
;;; #define PANGO_GRAVITY_IS_IMPROPER(gravity)
;;;
;;; Whether a PangoGravity represents a gravity that results in reversal of
;;; text direction.
;;;
;;; gravity :
;;;     the PangoGravity to check
;;;
;;; Returns :
;;;     TRUE if gravity is PANGO_GRAVITY_WEST or PANGO_GRAVITY_NORTH, FALSE
;;;     otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GRAVITY_IS_VERTICAL()
;;;
;;; #define PANGO_GRAVITY_IS_VERTICAL(gravity)
;;;
;;; Whether a PangoGravity represents vertical writing directions.
;;;
;;; gravity :
;;;     the PangoGravity to check
;;;
;;; Returns :
;;;     TRUE if gravity is PANGO_GRAVITY_EAST or PANGO_GRAVITY_WEST, FALSE
;;;     otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_gravity_get_for_matrix ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_gravity_get_for_matrix" gravity-for-matrix) gravity
 #+liber-documentation
 "@version{2024-2-22}
  @argument[matrix]{a @class{pango:matrix} instance}
  @begin{return}
    The gravity of @arg{matrix}, which will never be @code{:auto}, or
    @code{:south} if @arg{matrix} is @code{nil}.
  @end{return}
  @begin{short}
    Finds the gravity that best matches the rotation component in a
    @class{pango:matrix} instance.
  @end{short}
  @see-class{pango:matrix}"
  (matrix (g:boxed matrix)))

(export 'gravity-for-matrix)

;;; ----------------------------------------------------------------------------
;;; pango_gravity_get_for_script ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_gravity_get_for_script" gravity-for-script) gravity
 #+liber-documentation
 "@version{2024-2-22}
  @argument[script]{a @symbol{pango:script} value to query}
  @argument[gravity]{a base @symbol{pango:gravity} value of the paragraph}
  @argument[hint]{a @symbol{pango:gravity-hint} value with the orientation hint}
  @begin{return}
    Resolved @symbol{pango:gravity} value suitable to use for a run of text
    with @arg{script}.
  @end{return}
  @begin{short}
    Based on the script, base gravity, and hint, returns actual gravity to use
    in laying out a single @class{pango:item} instance.
  @end{short}
  If the @arg{gravity} argument is @code{:auto}, it is first replaced with the
  preferred gravity of @arg{script}. To get the preferred gravity of a script,
  pass the @code{:auto} and @code{:strong} values in.
  @see-class{pango:item}
  @see-symbol{pango:script}
  @see-symbol{pango:gravity}
  @see-symbol{pango:gravity-hint}"
  (script script)
  (gravity gravity)
  (hint gravity-hint))

(export 'gravity-for-script)

;;; ----------------------------------------------------------------------------
;;; pango_gravity_get_for_script_and_width ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_gravity_get_for_script_and_width"
                gravity-for-script-and-width) gravity
 #+liber-documentation
 "@version{2024-2-22}
  @argument[script]{a @symbol{pango:script} value to query}
  @argument[wide]{@em{true} for wide characters as returned by
    @code{g_unichar_iswide()}}
  @argument[gravity]{a base @symbol{pango:gravity} value of the paragraph}
  @argument[hint]{a @symbol{pango:gravity-hint} value with the orientation hint}
  @begin{return}
    Resolved @symbol{pango:gravity} value suitable to use for a run of text
    with @arg{script} and @arg{wide}.
  @end{return}
  @begin{short}
    Based on the script, East Asian width, base gravity, and hint, returns
    actual gravity to use in laying out a single character or a
    @class{pango:item} instance.
  @end{short}

  This function is similar to the @fun{pango:gravity-for-script} function
  except that this function makes a distinction between narrow/half-width and
  wide/full-width characters also. Wide/full-width characters always stand
  upright, that is, they always take the base gravity, whereas narrow/full-width
  characters are always rotated in vertical context.

  If the @arg{gravity} argument is @code{:auto}, it is first replaced with the
  preferred gravity of @arg{script}.
  @see-class{pango:item}
  @see-symbol{pango:script}
  @see-symbol{pango:gravity}
  @see-symbol{pango:gravity-hint}
  @see-function{pango:gravity-for-script}"
  (script script)
  (wide :boolean)
  (gravity gravity)
  (hint gravity-hint))

(export 'gravity-for-script-and-width)

;;; ----------------------------------------------------------------------------
;;; pango_gravity_to_rotation ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_gravity_to_rotation" gravity-to-rotation) :double
 #+liber-documentation
 "@version{2024-2-22}
  @argument[script]{a @symbol{pango:script} value to query}
  @begin{return}
    The double float with the rotation value corresponding to @arg{gravity}.
  @end{return}
  @begin{short}
    Converts a @symbol{pango:gravity} value to its natural rotation in radians.
  @end{short}
  The @arg{gravity} argument should not be the @code{:auto} value. Note that the
  @fun{pango:matrix-rotate} function takes angle in degrees, not radians. So,
  to call the @fun{pango:matrix-rotate} function with the output
  of this function you should multiply it by @code{(180 / pi)}.
  @see-symbol{pango:script}
  @see-symbol{pango:gravity}
  @see-function{pango:matrix-rotate}"
  (gravity gravity))

(export 'gravity-to-rotation)

;;; --- End of file pango.gravity.lisp -----------------------------------------
