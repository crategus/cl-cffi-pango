;;; ----------------------------------------------------------------------------
;;; pango.coverage.lisp
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
;;; Coverage Maps
;;;
;;;     Unicode character range coverage storage
;;;
;;; Types and Values
;;;
;;;     PangoCoverageLevel
;;;     PangoCoverage
;;;
;;; Functions
;;;
;;;     pango_coverage_new
;;;     pango_coverage_ref                                 Deprecated 1.52
;;;     pango_coverage_unref                               Deprecated 1.52
;;;     pango_coverage_copy                                not needed
;;;     pango_coverage_get
;;;     pango_coverage_max                                 Deprecated 1.44
;;;     pango_coverage_set
;;;     pango_coverage_to_bytes                            Deprecated 1.44
;;;     pango_coverage_from_bytes                          Deprecated 1.44
;;;
;;; Object Hierarchy
;;;
;;;     GEnum
;;;     ╰── PangoCoverageLevel
;;;
;;;     GObject
;;;     ╰── PangoCoverage
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoCoverageLevel
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoCoverageLevel" coverage-level
  (:export t
   :type-initializer "pango_coverage_level_get_type")
  (:none 0)
  (:fallback 1)
  (:approximate 2)
  (:exact 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'coverage-level)
      "GEnum"
      (liber:symbol-documentation 'coverage-level)
 "@version{2024-2-24}
  @begin{declaration}
(gobject:define-genum \"PangoCoverageLevel\" coverage-level
  (:export t
   :type-initializer \"pango_coverage_level_get_type\")
  (:none 0)
  (:fallback 1)
  (:approximate 2)
  (:exact 3))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{The character is not representable with the font.}
      @entry[:fallback]{The character is represented in a way that may be
        comprehensible but is not the correct graphical form. For instance, a
        Hangul character represented as a a sequence of Jamos, or a Latin
        transliteration of a Cyrillic word.}
      @entry[:approximage]{The character is represented as basically the correct
        graphical form, but with a stylistic variant inappropriate for the
        current script.}
      @entry[:exact]{The character is represented as the correct graphical
        form.}
    @end{table}
  @end{values}
  @begin{short}
    Used to indicate how well a font can represent a particular Unicode
    character point for a particular script.
  @end{short}
  Since 1.44, only the @code{:none} and @code{:exact} values will be returned.
  @see-symbol{pango:coverage}")

;;; ----------------------------------------------------------------------------
;;; PangoCoverage
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "PangoCoverage" coverage
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_coverage_get_type")
  nil)

#+liber-documentation
(setf (documentation 'coverage 'type)
 "@version{2024-2-24}
  @begin{short}
    The @symbol{pango:coverage} class represents a map from Unicode characters
    to @symbol{pango:coverage-level} values.
  @end{short}
  It is often necessary in Pango to determine if a particular font can represent
  a particular character, and also how well it can represent that character. The
  @class{pango:coverage} object is a data structure that is used to represent
  that information. It is an opaque structure with no public fields.
  @see-constructor{pango:coverage-new}
  @see-symbol{pango:coverage-level}")

;;; ----------------------------------------------------------------------------
;;; pango_coverage_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline coverage-new))

(defun coverage-new ()
 #+liber-documentation
 "@version{2024-2-24}
  @return{The newly allocated @class{pango:coverage} object.}
  @begin{short}
    Creates a new @class{pango:coverage} object initialized to the @code{:none}
    value of the @symbol{pango:coverage-level} enumeration.
  @end{short}
  @see-class{pango:coverage}
  @see-symbol{pango:coverage-level}"
  (make-instance 'coverage))

(export 'coverage-new)

;;; ----------------------------------------------------------------------------
;;; pango_coverage_ref ()
;;;
;;; PangoCoverage *
;;; pango_coverage_ref (PangoCoverage *coverage);
;;;
;;; Increase the reference count on the PangoCoverage by one
;;;
;;; coverage :
;;;     a PangoCoverage.
;;;
;;; Returns :
;;;     coverage .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_unref ()
;;;
;;; void
;;; pango_coverage_unref (PangoCoverage *coverage);
;;;
;;; Decrease the reference count on the PangoCoverage by one. If the result is
;;; zero, free the coverage and all associated memory.
;;;
;;; coverage :
;;;     a PangoCoverage.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_copy ()
;;;
;;; PangoCoverage *
;;; pango_coverage_copy (PangoCoverage *coverage);
;;;
;;; Copy an existing PangoCoverage. (This function may now be unnecessary since
;;; we refcount the structure. File a bug if you use it.)
;;;
;;; coverage :
;;;     a PangoCoverage
;;;
;;; Returns :
;;;     the newly allocated PangoCoverage, with a reference count of one, which
;;;     should be freed with pango_coverage_unref().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_get ()
;;; ----------------------------------------------------------------------------

;; TODO: Can we use the char code for a character as the index!?

(cffi:defcfun ("pango_coverage_get" coverage-get) coverage-level
 #+liber-documentation
 "@version{2024-3-4}
  @argument[coverage]{a @class{pango:coverage} object}
  @argument[index]{an integer with the index to check}
  @return{The @symbol{pango:coverage-level} value with the coverage level for
    the character with @arg{index}.}
  @begin{short}
    Determine whether a particular index is covered by @arg{coverage}.
  @end{short}
  @see-class{pango:coverage}
  @see-symbol{pango:coverage-level}
  @see-function{pango:coverage-set}"
  (coverage (g:object coverage))
  (index :int))

(export 'coverage-get)

;;; ----------------------------------------------------------------------------
;;; pango_coverage_max ()
;;;
;;; void
;;; pango_coverage_max (PangoCoverage *coverage,
;;;                     PangoCoverage *other);
;;;
;;; pango_coverage_max has been deprecated since version 1.44 and should not be
;;; used in newly written code.
;;;
;;; This function does nothing
;;;
;;; Set the coverage for each index in coverage to be the max (better) value of
;;; the current coverage for the index and the coverage for the corresponding
;;; index in other .
;;;
;;; coverage :
;;;     a PangoCoverage
;;;
;;; other :
;;;     another PangoCoverage
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_set ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_coverage_set" coverage-set) :void
 #+liber-documentation
 "@version{2024-2-24}
  @argument[coverage]{a @class{pango:coverage} object}
  @argument[index]{an integet with the index to modify}
  @argument[level]{a new @symbol{pango:coverage-level} value for @arg{index}}
  @begin{short}
    Modify a particular index within @arg{coverage}.
  @end{short}
  @see-class{pango:coverage}
  @see-symbol{pango:coverage-level}
  @see-function{pango:coverage-get}"
  (coverage (g:object coverage))
  (index :int)
  (level coverage-level))

(export 'coverage-set)

;;; ----------------------------------------------------------------------------
;;;pango_coverage_to_bytes ()
;;;
;;; void
;;; pango_coverage_to_bytes (PangoCoverage *coverage,
;;;                          guchar **bytes,
;;;                          int *n_bytes);
;;;
;;; pango_coverage_to_bytes has been deprecated since version 1.44 and should
;;; not be used in newly written code.
;;;
;;; This returns NULL
;;;
;;; Convert a PangoCoverage structure into a flat binary format
;;;
;;; coverage :
;;;     a PangoCoverage
;;;
;;; bytes :
;;;     location to store result (must be freed with g_free()).
;;;
;;; n_bytes :
;;;     location to store size of result.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_from_bytes ()
;;;
;;; PangoCoverage *
;;; pango_coverage_from_bytes (guchar *bytes,
;;;                            int n_bytes);
;;;
;;; pango_coverage_from_bytes has been deprecated since version 1.44 and should
;;; not be used in newly written code.
;;;
;;; This returns NULL
;;;
;;; Convert data generated from pango_coverage_to_bytes() back to a
;;; PangoCoverage
;;;
;;; bytes :
;;;     binary data representing a PangoCoverage.
;;;
;;; n_bytes :
;;;     the size of bytes in bytes
;;;
;;; Returns :
;;;     a newly allocated PangoCoverage, or NULL if the data was invalid.
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.coverage.lisp ----------------------------------------
