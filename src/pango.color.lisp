;;; ----------------------------------------------------------------------------
;;; pango.color.lisp
;;;
;;; The documentation in this file is taken from the Pango Reference Manual
;;; Version 1.56 and modified to document the Lisp binding to the Pango
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
;;; Types and Values
;;;
;;;     PangoColor
;;;
;;; Functions
;;;
;;;     pango_color_copy
;;;     pango_color_free                                    not implemented
;;;     pango_color_to_string
;;;     pango_color_parse
;;;     pango_color_parse_with_alpha
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── PangoColor
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoColor
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct color "PangoColor"
  (:export t
   :type-initializer "pango_color_get_type")
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))

#+liber-documentation
(setf (liber:alias-for-class 'color)
      "GBoxed"
      (documentation 'color 'type)
 "@version{2025-08-24}
  @begin{declaration}
(glib:define-gboxed-cstruct color \"PangoColor\"
  (:export t
   :type-initializer \"pango_color_get_type\")
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[red]{The unsigned integer for the red component of the color.}
      @entry[green]{The unsigned integer for the green component of the color.}
      @entry[blue]{The unsigned integer for the blue component of the color.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @class{pango:color} structure is used to represent a color in an
    uncalibrated RGB color space.
  @end{short}
  @see-constructor{pango:color-new}
  @see-constructor{pango:color-copy}
  @see-constructor{pango:color-parse}
  @see-constructor{pango:color-parse-with-alpha}
  @see-slot{pango:color-red}
  @see-slot{pango:color-green}
  @see-slot{pango:color-red}")

;;; --- pango:color-red --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'color-red)
      "Accessor"
      (documentation 'color-red 'function)
 "@version{2025-08-24}
  @syntax{(pango:color-red instance) => red}
  @syntax{(setf (pango:color-red instance) red)}
  @begin{short}
    The accessor for the @code{red} slot of the @class{pango:color} color
    gets or sets the red component of the color.
  @end{short}
  This is a value between 0 and 65535, with 65535 indicating full intensity.
  @see-class{pango:color}")

;;; --- pango:color-green ------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'color-green)
      "Accessor"
      (documentation 'color-green 'function)
 "@version{2025-08-24}
  @syntax{(pango:color-green instance) => green}
  @syntax{(setf (pango:color-green instance) green)}
  @begin{short}
    The accessor for the @code{green} slot of the @class{pango:color} color
    gets or sets the green component of the color.
  @end{short}
  This is a value between 0 and 65535, with 65535 indicating full intensity.
  @see-class{pango:color}")

;;; --- pango:color-blue -------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'color-blue)
      "Accessor"
      (documentation 'color-blue 'function)
 "@version{2025-08-24}
  @syntax{(pango:color-blue instance) => blue}
  @syntax{(setf (pango:color-blue instance) blue)}
  @begin{short}
    The accessor for the @code{blue} slot of the @class{pango:color} color
    gets or sets the blue component of the color.
  @end{short}
  This is a value between 0 and 65535, with 65535 indicating full intensity.
  @see-class{pango:color}")

;;; ----------------------------------------------------------------------------
;;; pango:color-new
;;; ----------------------------------------------------------------------------

(declaim (inline color-new))

(defun color-new (&key (red 0) (green 0) (blue 0))
 #+liber-documentation
 "@version{2025-02-15}
  @argument[red]{an unsigned integer for the red component of the color}
  @argument[green]{an unsigned integer for the green component of the color}
  @argument[blue]{an unsigned integer for the blue component of the color}
  @begin{short}
    Creates a new @class{pango:color} color.
  @end{short}
  The values are between 0 and 65535, with 65535 indicating full intensity.
  @see-class{pango:color}
  @see-function{pango:color-copy}"
  (make-color :red red :green green :blue blue))

(export 'color-new)

;;; ----------------------------------------------------------------------------
;;; pango_color_copy
;;; ----------------------------------------------------------------------------

(declaim (inline color-copy))

(defun color-copy (color)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[color]{a @sym{pango:color} instance}
  @return{The newly created @class{pango:color} instance.}
  @short{Creates a copy of @arg{color}.}
  @see-class{pango:color}
  @see-function{pango:color-new}"
  (copy-color color))

(export 'color-copy)

;;; ----------------------------------------------------------------------------
;;; pango_color_free
;;;
;;; Frees a color allocated by pango_color_copy().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_color_parse
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_color_parse" %color-parse) :boolean
  (color (g:boxed color))
  (spec :string))

(defun color-parse (spec)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[spec]{a string specifying a color}
  @begin{return}
    The newly created @class{pango:color} instance for the result, or
    @code{nil}.
  @end{return}
  @begin{short}
    Fill in the fields of a color from a string specification.
  @end{short}
  The string can either one of a large set of standard names, taken from the
  X11 @file{rgb.txt} file, or it can be a hex value in the form @code{#rgb},
  @code{#rrggbb}, @code{#rrrgggbbb} or @code{#rrrrggggbbbb} where @code{r},
  @code{g}, and @code{b} are hex digits of the red, green, and blue components
  of the color, respectively. White in the four forms is @code{#fff},
  @code{#ffffff}, @code{#fffffffff} and @code{#ffffffffffff}.
  @begin[Examples]{dictionary}
    @begin{pre}
(pango:color-parse \"blue\")
=> #S(PANGO:COLOR :RED 0 :GREEN 0 :BLUE 65535)
(pango:color-to-string *)
=> \"#00000000ffff\"
(pango:color-parse \"white\")
=> #S(PANGO:COLOR :RED 65535 :GREEN 65535 :BLUE 65535)
(pango:color-to-string *)
=> \"#ffffffffffff\"
    @end{pre}
  @end{dictionary}
  @see-class{pango:color}
  @see-function{pango:color-to-string}"
  (let ((color (color-new)))
    (when (%color-parse color spec)
      color)))

(export 'color-parse)

;;; ----------------------------------------------------------------------------
;;; pango_color_parse_with_alpha
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_color_parse_with_alpha" %color-parse-with-alpha) :boolean
  (color (g:boxed color))
  (alpha (:pointer :uint16))
  (spec :string))

(defun color-parse-with-alpha (spec)
 #+liber-documentation
 "@version{2025-08-24}
  @syntax{(pango:color-parse-with-alpha spec) => color, alpha}
  @argument[spec]{a string specifying a color}
  @argument[color]{a newly created @class{pango:color} instance for the result,
    or @code{nil}}
  @argument[alpha]{an unsigned integer for the alpha value}
  @begin{short}
    Fill in the fields of a color from a string specification.
  @end{short}
  The string can either one of a large set of standard names, taken from the
  CSS specification, or it can be a hexadecimal value in the form @code{#rgb},
  @code{#rrggbb}, @code{#rrrgggbbb} or @code{#rrrrggggbbbb} where @code{r},
  @code{g} and @code{b} are hex digits of the red, green, and blue components
  of the color, respectively. White in the four forms is @code{#fff},
  @code{#ffffff}, @code{#fffffffff} and @code{#ffffffffffff}.

  Additionally, parse strings of the form @code{#rgba}, @code{#rrggbbaa},
  @code{#rrrrggggbbbbaaaa}, and set alpha to the value specified by the hex
  digits for @code{a}. If no alpha component is found in @arg{spec}, alpha is
  set to @code{0xffff}, for a solid color.
  @see-class{pango:color}
  @see-function{pango:color-parse}"
  (let ((color (make-color)))
    (cffi:with-foreign-object (alpha :uint16)
      (when (%color-parse-with-alpha color alpha spec)
        (values color (cffi:mem-ref alpha :uint16))))))

(export 'color-parse-with-alpha)

;;; ----------------------------------------------------------------------------
;;; pango_color_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_color_to_string" color-to-string) :string
 #+liber-documentation
 "@version{2025-08-24}
  @argument[color]{a @class{pango:color} instance}
  @return{The string for the hexadecimal form of @arg{color}.}
  @begin{short}
    Returns a textual specification of @arg{color} in the hexadecimal form
    @code{#rrrrggggbbbb}, where @code{r}, @code{g} and @code{b} are hex digits
    representing the red, green, and blue components respectively.
  @end{short}
  @see-class{pango:color}
  @see-function{pango:color-parse}"
  (color (g:boxed color)))

(export 'color-to-string)

;;; --- End of file pango.color.lisp -------------------------------------------
