;;; ----------------------------------------------------------------------------
;;; pango.bidirectional.lisp
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
;;; Bidirectional Text
;;;
;;;     Types and functions to help with handling bidirectional text
;;;
;;; Types and Value
;;;
;;;     PangoDirection
;;;     PangoBidiType                                      Deprecated 1.44
;;;
;;; Function
;;;
;;;     pango_unichar_direction
;;;     pango_find_base_dir
;;;     pango_get_mirror_char                              Deprecated 1.30
;;;     pango_bidi_type_for_unichar                        Deprecated 1.44
;;;
;;; Object Hierarchy
;;;
;;;     GEnum
;;;     ├── PangoBidiType
;;;     ╰── PangoDirection
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoDirection
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoDirection" direction
  (:export t
   :type-initializer "pango_direction_get_type")
  (:ltr 0)
  (:rtl 1)
  (:ttb-ltr 2)
  (:ttb-rtl 3)
  (:weak-ltr 4)
  (:weak-rtl 5)
  (:neutral 6))

#+liber-documentation
(setf (liber:alias-for-symbol 'direction)
      "GEnum"
      (liber:symbol-documentation 'direction)
 "@version{2024-2-25}
  @begin{short}
    The @symbol{pango:direction} enumeration represents a direction in the
    Unicode bidirectional algorithm.
  @end{short}
  Not every value in this enumeration makes sense for every usage. For example,
  the return value of the @fun{pango:unichar-direction} and
  @fun{pango:find-base-dir} functions cannot be @code{:weak-ltr} or
  @code{:weak-rtl}, since every character is either neutral or has a strong
  direction. On the other hand the @code{:neutral} value does not make sense to
  pass to the @fun{pango:itemize-with-base-dir} function.

  The @code{:ttb-ltr}, @code{:ttb-rtl} values come from an earlier
  interpretation of this enumeration as the writing direction of a block of
  text and are no longer used. See the @symbol{pango:gravity} enumeration for
  how vertical text is handled in Pango.
  @begin{pre}
(gobject:define-g-enum \"PangoDirection\" direction
  (:export t
   :type-initializer \"pango_direction_get_type\")
  (:ltr 0)
  (:rtl 1)
  (:ttb-ltr 2)
  (:ttb-rtl 3)
  (:weak-ltr 4)
  (:weak-rtl 5)
  (:neutral 6))
  @end{pre}
  @begin[code]{table}
    @entry[:ltr]{A strong left-to-right direction.}
    @entry[:rtl]{A strong right-to-left direction.}
    @entry[:ttb-ltr]{Deprecated value; treated the same as @code{:rtl}.}
    @entry[:ttb-rtl]{Deprecated value; treated the same as @code{:ltr}.}
    @entry[:weak-ltr]{A weak left-to-right direction.}
    @entry[:wek-rtl]{A weak right-to-left direction.}
    @entry[:neutral]{No direction specified.}
  @end{table}
  @see-symbol{pango:gravity}
  @see-function{pango:unichar-direction}
  @see-function{pango:find-base-dir}
  @see-function{pango:itemize-with-base-dir}")

;;; ----------------------------------------------------------------------------
;;; enum PangoBidiType
;;;
;;; typedef enum {
;;;   /* Strong types */
;;;   PANGO_BIDI_TYPE_L,
;;;   PANGO_BIDI_TYPE_LRE,
;;;   PANGO_BIDI_TYPE_LRO,
;;;   PANGO_BIDI_TYPE_R,
;;;   PANGO_BIDI_TYPE_AL,
;;;   PANGO_BIDI_TYPE_RLE,
;;;   PANGO_BIDI_TYPE_RLO,
;;;
;;;   /* Weak types */
;;;   PANGO_BIDI_TYPE_PDF,
;;;   PANGO_BIDI_TYPE_EN,
;;;   PANGO_BIDI_TYPE_ES,
;;;   PANGO_BIDI_TYPE_ET,
;;;   PANGO_BIDI_TYPE_AN,
;;;   PANGO_BIDI_TYPE_CS,
;;;   PANGO_BIDI_TYPE_NSM,
;;;   PANGO_BIDI_TYPE_BN,
;;;
;;;   /* Neutral types */
;;;   PANGO_BIDI_TYPE_B,
;;;   PANGO_BIDI_TYPE_S,
;;;   PANGO_BIDI_TYPE_WS,
;;;   PANGO_BIDI_TYPE_ON
;;; } PangoBidiType;
;;;
;;; The PangoBidiType type represents the bidirectional character type of a
;;; Unicode character as specified by the Unicode bidirectional algorithm.
;;;
;;; Warning
;;;
;;; PangoBidiType has been deprecated since version 1.44 and should not be used
;;; in newly written code. Use fribidi for this information
;;;
;;; PANGO_BIDI_TYPE_L
;;;     Left-to-Right
;;;
;;; PANGO_BIDI_TYPE_LRE
;;;     Left-to-Right Embedding
;;;
;;; PANGO_BIDI_TYPE_LRO
;;;     Left-to-Right Override
;;;
;;; PANGO_BIDI_TYPE_R
;;;     Right-to-Left
;;;
;;; PANGO_BIDI_TYPE_AL
;;;     Right-to-Left Arabic
;;;
;;; PANGO_BIDI_TYPE_RLE
;;;     Right-to-Left Embedding
;;;
;;; PANGO_BIDI_TYPE_RLO
;;;     Right-to-Left Override
;;;
;;; PANGO_BIDI_TYPE_PDF
;;;     Pop Directional Format
;;;
;;; PANGO_BIDI_TYPE_EN
;;;     European Number
;;;
;;; PANGO_BIDI_TYPE_ES
;;;     European Number Separator
;;;
;;; PANGO_BIDI_TYPE_ET
;;;     European Number Terminator
;;;
;;; PANGO_BIDI_TYPE_AN
;;;     Arabic Number
;;;
;;; PANGO_BIDI_TYPE_CS
;;;     Common Number Separator
;;;
;;; PANGO_BIDI_TYPE_NSM
;;;     Nonspacing Mark
;;;
;;; PANGO_BIDI_TYPE_BN
;;;     Boundary Neutral
;;;
;;; PANGO_BIDI_TYPE_B
;;;     Paragraph Separator
;;;
;;; PANGO_BIDI_TYPE_S
;;;     Segment Separator
;;;
;;; PANGO_BIDI_TYPE_WS
;;;     Whitespace
;;;
;;; PANGO_BIDI_TYPE_ON
;;;     Other Neutrals
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_unichar_direction ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_unichar_direction" unichar-direction) direction
 #+liber-documentation
 "@version{2024-2-25}
  @argument[ch]{a Lisp character}
  @return{The @symbol{pango:direction} value with the direction of the
    characer.}
  @begin{short}
    Determines the inherent direction of a character.
  @end{short}
  Either @code{:ltr}, @code{:rtl}, or @code{:neutral}. This function is useful
  to categorize characters into left-to-right letters, right-to-left letters,
  and everything else.
  @see-symbol{pango:direction}"
  (ch g:unichar))

(export 'unichar-direction)

;;; ----------------------------------------------------------------------------
;;; pango_find_base_dir ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_find_base_dir" %find-base-dir) direction
  (text :string)
  (len :int))

(defun find-base-dir (text)
 #+liber-documentation
 "@version{2024-2-25}
  @argument[text]{a string with text to process}
  @return{The @symbol{pango:direction} value with the direction corresponding
    to the first strong character. If no such character is found, then the
    @code{:neutral} value is returned.}
  @begin{short}
    Searches a string for the first character that has a strong direction,
    according to the Unicode bidirectional algorithm.
  @end{short}
  @see-symbol{pango:direction}"
  (%find-base-dir text (length text)))

(export 'find-base-dir)

;;; ----------------------------------------------------------------------------
;;; pango_get_mirror_char ()
;;;
;;; gboolean pango_get_mirror_char (gunichar ch, gunichar *mirrored_ch);
;;;
;;; Warning
;;;
;;; pango_get_mirror_char is deprecated and should not be used in newly written
;;; code.
;;;
;;; If ch has the Unicode mirrored property and there is another Unicode
;;; character that typically has a glyph that is the mirror image of ch's glyph,
;;; puts that character in the address pointed to by mirrored_ch.
;;;
;;; Use g_unichar_get_mirror_char() instead; the docs for that function provide
;;; full details.
;;;
;;; Warning
;;;
;;; pango_get_mirror_char is deprecated and should not be used in newly written
;;; code.
;;;
;;; ch :
;;;     a Unicode character
;;;
;;; mirrored_ch :
;;;     location to store the mirrored character
;;;
;;; Returns :
;;;     TRUE if ch has a mirrored character and mirrored_ch is filled in, FALSE
;;;     otherwise.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_bidi_type_for_unichar ()
;;;
;;; PangoBidiType pango_bidi_type_for_unichar (gunichar ch);
;;;
;;; Determines the normative bidirectional character type of a character, as
;;; specified in the Unicode Character Database.
;;;
;;; A simplified version of this function is available as
;;; pango_unichar_get_direction().
;;;
;;; Warning
;;;
;;; pango_bidi_type_for_unichar is deprecated and should not be used in
;;; newly written code.
;;;
;;; ch :
;;;     a Unicode character
;;;
;;; Returns :
;;;     the bidirectional character type, as used in the Unicode bidirectional
;;;     algorithm.
;;; ----------------------------------------------------------------------------

;;; --- pango.bidirectional.lisp -----------------------------------------------
