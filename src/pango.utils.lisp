;;; ----------------------------------------------------------------------------
;;; pango.utils.lisp
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
;;;     PANGO_SCALE
;;;
;;; Functions
;;;
;;;     PANGO_PIXELS
;;;     PANGO_PIXELS_FLOOR
;;;     PANGO_PIXELS_CEIL
;;;     PANGO_UNITS_ROUND
;;;
;;;     pango_units_to_double
;;;     pango_units_from_double
;;;
;;;     PANGO_ASCENT
;;;     PANGO_DESCENT
;;;     PANGO_LBEARING
;;;     PANGO_RBEARING
;;;
;;;     pango_extents_to_pixels
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PANGO_SCALE
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+scale+) "Constant")

(defconstant +scale+ 1024
 #+liber-documentation
 "@version{2024-3-3}
  @variable-value{1024}
  @begin{short}
    The @var{pango:+scale+} constant represents the scale between dimensions
    used for Pango distances and device units.
  @end{short}
  The definition of device units is dependent on the output device. It will
  typically be pixels for a screen, and points for a printer.
  The @var{pango:+scale+} value is currently 1024, but this may be changed in
  the future.

  When setting font sizes, device units are always considered to be points as
  in \"12 point font\", rather than pixels.
  @see-function{pango:pixels}")

(export '+scale+)

;;; ----------------------------------------------------------------------------
;;; PANGO_PIXELS()
;;; ----------------------------------------------------------------------------

(defun pixels (d)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[d]{a dimension in Pango units}
  @return{Rounded dimension in device units.}
  @begin{short}
    Converts a dimension to device units by rounding.
  @end{short}
  @see-variable{pango:+scale+}"
  (ash (+ d 512) -10)) ; #define PANGO_PIXELS(d) (((int)(d) + 512) >> 10)

(export 'pixels)

;;; ----------------------------------------------------------------------------
;;; PANGO_PIXELS_FLOOR()
;;;
;;; #define PANGO_PIXELS_FLOOR(d) (((int)(d)) >> 10)
;;;
;;; Converts a dimension to device units by flooring.
;;;
;;; d :
;;;     a dimension in Pango units.
;;;
;;; Returns :
;;;     floored dimension in device units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_PIXELS_CEIL()
;;;
;;; #define PANGO_PIXELS_CEIL(d) (((int)(d) + 1023) >> 10)
;;;
;;; Converts a dimension to device units by ceiling.
;;;
;;; d :
;;;     a dimension in Pango units.
;;;
;;; Returns :
;;;     ceiled dimension in device units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_UNITS_ROUND()
;;;
;;; #define PANGO_UNITS_ROUND(d)
;;;
;;; Rounds a dimension to whole device units, but does not convert it to device
;;; units.
;;;
;;; d :
;;;     a dimension in Pango units.
;;;
;;; Returns :
;;;     rounded dimension in Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_units_to_double ()
;;;
;;; double pango_units_to_double (int i);
;;;
;;; Converts a number in Pango units to floating-point: divides it by
;;; PANGO_SCALE.
;;;
;;; i :
;;;     value in Pango units
;;;
;;; Returns :
;;;     the double value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_units_from_double ()
;;;
;;; int pango_units_from_double (double d);
;;;
;;; Converts a floating-point number to Pango units: multiplies it by
;;; PANGO_SCALE and rounds to nearest integer.
;;;
;;; d :
;;;     double floating-point value
;;;
;;; Returns :
;;;     the value in Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ASCENT()
;;; ----------------------------------------------------------------------------

(declaim (inline ascent))

(defun ascent (rect)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[rect]{a @symbol{pango:rectangle} instance}
  @begin{short}
    Extracts the ascent from a @symbol{pango:rectangle} instance representing
    glyph extents.
  @end{short}
  The ascent is the distance from the baseline to the highest point of the
  character. This is positive if the glyph ascends above the baseline.
  @see-symbol{pango:rectangle}"
  (- (pango:rectangle-y rect)))

(export 'ascent)

;;; ----------------------------------------------------------------------------
;;; PANGO_DESCENT()
;;; ----------------------------------------------------------------------------

(declaim (inline descent))

(defun descent (rect)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[rect]{a @symbol{pango:rectangle} instance}
  @begin{short}
    Extracts the descent from a @symbol{pango:rectangle} instance representing
    glyph extents.
  @end{short}
  The descent is the distance from the baseline to the lowest point of the
  character. This is positive if the glyph descends below the baseline.
  @see-symbol{pango:rectangle}"
  (+ (rectangle-y  rect) (rectangle-height rect)))

(export 'descent)

;;; ----------------------------------------------------------------------------
;;; PANGO_LBEARING()
;;; ----------------------------------------------------------------------------

(declaim (inline lbearing))

(defun lbearing (rect)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[rect]{a @symbol{pango:rectangle} instance}
  @begin{short}
    Extracts the left bearing from a @symbol{pango:rectangle} instance
    representing glyph extents.
  @end{short}
  The left bearing is the distance from the horizontal origin to the farthest
  left point of the character. This is positive for characters drawn completely
  to the right of the glyph origin.
  @see-symbol{pango:rectangle}"
  (rectangle-x rect))

(export 'lbearing)

;;; ----------------------------------------------------------------------------
;;; PANGO_RBEARING()
;;; ----------------------------------------------------------------------------

(declaim (inline rbearing))

(defun rbearing (rect)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[rect]{a @symbol{pango:rectangle} instance}
  @begin{short}
    Extracts the right bearing from a @symbol{pango:rectangle} instance
    representing glyph extents.
  @end{short}
  The right bearing is the distance from the horizontal origin to the farthest
  right point of the character. This is positive except for characters drawn
  completely to the left of the horizontal origin.
  @see-symbol{pango:rectangle}"
  (+ (rectangle-x rect) (rectangle-width rect)))

(export 'rbearing)

;;; ----------------------------------------------------------------------------
;;; pango_extents_to_pixels ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_extents_to_pixels" extents-to-pixels) :void
 #+liber-documentation
 "@version{2023-12-25}
  @argument[inclusive]{a @symbol{pango:rectangle} instance to round to
    pixels inclusively, or @code{NULL}}
  @argument[nearest]{a @symbol{pango:rectangle} instance to round to
    nearest pixels, or @code{NULL}}
  @begin{short}
    Converts extents from Pango units to device units, dividing by the
    @var{pango:+scale+} factor and performing rounding.
  @end{short}

  The @arg{inclusive} rectangle is converted by flooring the x/y coordinates
  and extending width/height, such that the final rectangle completely includes
  the original rectangle.

  The @arg{nearest} rectangle is converted by rounding the coordinates of the
  rectangle to the nearest device unit (pixel).

  The rule to which argument to use is: if you want the resulting device-space
  rectangle to completely contain the original rectangle, pass it in as
  inclusive. If you want two touching-but-not-overlapping rectangles stay
  touching-but-not-overlapping after rounding to device units, pass them in as
  nearest.
  @see-symbol{pango:rectangle}
  @see-variable{pango:+scale+}"
  (inclusive (:pointer (:struct rectangle)))
  (nearest (:pointer (:struct rectangle))))

(export 'extents-to-pixels)

;;; --- End of file pango.utils.lisp -------------------------------------------
