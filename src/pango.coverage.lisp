;;; ----------------------------------------------------------------------------
;;; pango.coverage.lisp
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
;;;     pango_coverage_ref                                  Deprecated 1.52
;;;     pango_coverage_unref                                Deprecated 1.52
;;;     pango_coverage_copy                                 not needed
;;;     pango_coverage_get
;;;     pango_coverage_max                                  Deprecated 1.44
;;;     pango_coverage_set
;;;     pango_coverage_to_bytes                             Deprecated 1.44
;;;     pango_coverage_from_bytes                           Deprecated 1.44
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
 "@version{2025-08-24}
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
    @begin[code]{simple-table}
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
    @end{simple-table}
  @end{values}
  @begin{short}
    Used to indicate how well a font can represent a particular Unicode
    character point for a particular script.
  @end{short}
  Only the @val[pango:coverage-level]{:none} and
  @val[pango:coverage-level]{:exact} values will be returned.
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
 "@version{2025-08-24}
  @begin{short}
    The @sym{pango:coverage} class represents a map from Unicode characters to
    @sym{pango:coverage-level} values.
  @end{short}
  It is often necessary in Pango to determine if a particular font can represent
  a particular character, and also how well it can represent that character. The
  @class{pango:coverage} object is a data structure that is used to represent
  that information. It is an opaque structure with no public fields.
  @see-constructor{pango:coverage-new}
  @see-symbol{pango:coverage-level}")

;;; ----------------------------------------------------------------------------
;;; pango_coverage_new
;;; ----------------------------------------------------------------------------

(declaim (inline coverage-new))

(defun coverage-new ()
 #+liber-documentation
 "@version{2025-08-24}
  @return{The newly allocated @class{pango:coverage} object.}
  @begin{short}
    Creates a new @class{pango:coverage} object initialized to the
    @val[pango:coverage-level]{:none} value of the @sym{pango:coverage-level}
    enumeration.
  @end{short}
  @see-class{pango:coverage}
  @see-symbol{pango:coverage-level}"
  (make-instance 'coverage))

(export 'coverage-new)

;;; ----------------------------------------------------------------------------
;;; pango_coverage_ref
;;;
;;; Increase the reference count on the PangoCoverage by one
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_unref
;;;
;;; Decrease the reference count on the PangoCoverage by one. If the result is
;;; zero, free the coverage and all associated memory.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_copy
;;;
;;; Copy an existing PangoCoverage.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_get
;;; ----------------------------------------------------------------------------

;; TODO: Can we use the char code for a character as the index!?

(cffi:defcfun ("pango_coverage_get" coverage-get) coverage-level
 #+liber-documentation
 "@version{2025-08-24}
  @argument[coverage]{a @class{pango:coverage} object}
  @argument[index]{an integer for the index to check}
  @begin{return}
    The @sym{pango:coverage-level} value for the coverage level for the
    character with @arg{index}.
  @end{return}
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
;;; pango_coverage_max
;;;
;;; pango_coverage_max has been deprecated since version 1.44 and should not be
;;; used in newly written code.
;;;
;;; This function does nothing
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_set
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_coverage_set" coverage-set) :void
 #+liber-documentation
 "@version{2025-08-24}
  @argument[coverage]{a @class{pango:coverage} object}
  @argument[index]{an integer for the index to modify}
  @argument[level]{a new @sym{pango:coverage-level} value for @arg{index}}
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
;;;pango_coverage_to_bytes
;;;
;;; pango_coverage_to_bytes has been deprecated since version 1.44 and should
;;; not be used in newly written code.
;;;
;;; This returns NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_coverage_from_bytes
;;;
;;; pango_coverage_from_bytes has been deprecated since version 1.44 and should
;;; not be used in newly written code.
;;;
;;; This returns NULL
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.coverage.lisp ----------------------------------------
