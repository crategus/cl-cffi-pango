;;; ----------------------------------------------------------------------------
;;; pango.version.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.50 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; Version Checking
;;;
;;;     Tools for checking Pango version at compile- and run-time.
;;;
;;; Types and Values
;;;
;;;     PANGO_VERSION_ENCODE
;;;     PANGO_VERSION
;;;     PANGO_VERSION_MAJOR
;;;     PANGO_VERSION_MINOR
;;;     PANGO_VERSION_MICRO
;;;     PANGO_VERSION_STRING
;;;     PANGO_VERSION_CHECK
;;;
;;; Functions
;;;
;;;     pango_version
;;;     pango_version_string
;;;     pango_version_check
;;;
;;; Description
;;;
;;; The capital-letter macros defined here can be used to check the version of
;;; Pango at compile-time, and to encode Pango versions into integers. The
;;; functions can be used to check the version of the linked Pango library at
;;; run-time.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_ENCODE()
;;;
;;; #define PANGO_VERSION_ENCODE(major, minor, micro)
;;;
;;; This macro encodes the given Pango version into an integer. The numbers
;;; returned by PANGO_VERSION and pango_version() are encoded using this macro.
;;; Two encoded version numbers can be compared as integers.
;;;
;;; major :
;;;     the major component of the version number
;;;
;;; minor :
;;;     the minor component of the version number
;;;
;;; micro :
;;;     the micro component of the version number
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION
;;;
;;; #define PANGO_VERSION
;;;
;;; The version of Pango available at compile-time, encoded using
;;; PANGO_VERSION_ENCODE().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_MAJOR
;;;
;;; #define PANGO_VERSION_MAJOR 1
;;;
;;; The major component of the version of Pango available at compile-time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_MINOR
;;;
;;; #define PANGO_VERSION_MINOR 30
;;;
;;; The minor component of the version of Pango available at compile-time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_MICRO
;;;
;;; #define PANGO_VERSION_MICRO 0
;;;
;;; The micro component of the version of Pango available at compile-time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_STRING
;;;
;;; #define PANGO_VERSION_STRING "1.30.0"
;;;
;;; A string literal containing the version of Pango available at compile-time.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_VERSION_CHECK()
;;;
;;; #define PANGO_VERSION_CHECK(major,minor,micro)
;;;
;;; Checks that the version of Pango available at compile-time is not older
;;; than the provided version number.
;;;
;;; major :
;;;     the major component of the version number
;;;
;;; minor :
;;;     the minor component of the version number
;;;
;;; micro :
;;;     the micro component of the version number
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_version ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_version" version) :int
 #+liber-documentation
 "@version{2023-2-11}
  @return{An integer with the encoded version of the Pango library available at
    run time.}
  @begin{short}
    Returns the encoded version of Pango available at run-time.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(pango:version) => 15006
    @end{pre}
  @end{dictionary}
  @see-function{pango:version-string}")

(export 'version)

;;; ----------------------------------------------------------------------------
;;; pango_version_string ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_version_string" version-string) :string
 #+liber-documentation
 "@version{2023-2-11}
  @begin{return}
    A string containing the version of the Pango library available at run
    time.
  @end{return}
  @begin{short}
    Returns the version of Pango available at run-time
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(pango:version-string) => \"1.50.6\"
    @end{pre}
  @end{dictionary}
  @see-function{pango:version}")

(export 'version-string)

;;; ----------------------------------------------------------------------------
;;; pango_version_check ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_version_check" version-check) :string
 #+liber-documentation
 "@version{2023-2-11}
  @argument[required-major]{an integer with the required major version}
  @argument[required-minor]{an integer with the required minor version}
  @argument[required-micro]{an integer with the required major version}
  @begin{return}
    Returns @code{nil} if the Pango library is compatible with the given
    version, or a string describing the version mismatch.
  @end{return}
  @begin{short}
    Checks that the Pango library in use is compatible with the given version.
  @end{short}
  Compatibility is defined by two things: first the version of the running
  library is newer than the
  @arg{required-major}.@arg{required-minor}.@arg{required-micro} version.
  Second the running library must be binary compatible with the
  @arg{required-major}.@arg{required-minor}.@arg{required-micro} version
  (same major version.)
  @begin[Example]{dictionary}
    @begin{pre}
(pango:version-check 1 48 0) => NIL
(pango:version-check 1 52 0) => \"Pango version too old (micro mismatch)\"
    @end{pre}
  @end{dictionary}
  @see-function{pango:version}
  @see-function{pango:version-string}"
  (required-major :int)
  (required-minor :int)
  (required-micro :int))

(export 'version-check)

;;; --- End of file pango.version.lisp -----------------------------------------
