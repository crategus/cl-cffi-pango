;;; ----------------------------------------------------------------------------
;;; pango.markup.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.51 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2024 Dieter Kaiser
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
;;; Markup
;;;
;;;     Simple markup language for text with attributes
;;;
;;; Functions
;;;
;;;     pango_parse_markup
;;;     pango_markup_parser_new
;;;     pango_markup_parser_finish
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; pango_parse_markup ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_parse_markup" %parse-markup) :boolean
  (markup :string)
  (length :int)
  (marker g:unichar)
  (attrlist :pointer)
  (text (:pointer :string))
  (char (:pointer g:unichar))
  (err :pointer))

(defun parse-markup (markup marker)
 #+liber-documentation
 "@version{2024-2-27}
  @argument[markup]{a string with the parkup to parse}
  @argument[marker]{a character that precedes an accelerator, or 0 for none}
  @begin{return}
    @arg{text} -- a string with the text with tags stripped @br{}
    @arg{attrlist} -- a @symbol{pango:attr-list} instance @br{}
    @arg{char} -- a character with the accelerator char
  @end{return}
  @begin{short}
    Parses marked-up text to create a plain-text string and an attribute list.
  @end{short}
  If @arg{marker} is nonzero, the given character will mark the character
  following it as an accelerator. For example, @arg{marker} might be an
  ampersand or underscore. All characters marked as an accelerator will receive
  a @code{:underline-low} attribute, and the first character so marked will be
  returned in @arg{char}. Two @arg{marker} characters following each other
  produce a single literal @arg{marker} character.

  If any error happens, @code{nil} is returned.
  @begin[Examples]{dictionary}
    @begin{pre}
(multiple-value-bind (text attrlist char)
    (pango:parse-markup \"<b>_Text</b>\" #\\_)
  (values text
          (pango:attr-list-to-string attrlist)
          char))
=> \"Text\"
   \"0 1 underline low
    0 4 weight bold\"
   #\\T

(multiple-value-bind (text attrlist char)
    (pango:parse-markup \"<span foreground='blue' size='x-large'>
                           Blue text
                         </span>
                         is <i>cool</i>!\"
                        #\\_)
  (values text
          (pango:attr-list-to-string attrlist)
          char))
=> \"Blue text is cool!\"
   \"0 9 scale 1.440000
    0 9 foreground #00000000ffff
    13 17 style italic\"
   #\\Nul
    @end{pre}
  @end{dictionary}
  @see-class{pango:attr-list}"
  (glib:with-g-error (err)
    (cffi:with-foreign-objects ((text :string)
                                (char 'g:unichar)
                                (attrlist :pointer))
      (when (%parse-markup markup -1 marker attrlist text char err)
        (values (cffi:mem-ref text :string)
                (cffi:mem-ref attrlist '(g:boxed attr-list :return))
                (cffi:mem-ref char 'g:unichar))))))

(export 'parse-markup)

;;; ----------------------------------------------------------------------------
;;; pango_markup_parser_new ()
;;;
;;; GMarkupParseContext *
;;; pango_markup_parser_new (gunichar accel_marker);
;;;
;;; Parses marked-up text (see markup format) to create a plain-text string and
;;; an attribute list.
;;;
;;; If accel_marker is nonzero, the given character will mark the character
;;; following it as an accelerator. For example, accel_marker might be an
;;; ampersand or underscore. All characters marked as an accelerator will
;;; receive a PANGO_UNDERLINE_LOW attribute, and the first character so marked
;;; will be returned in accel_char , when calling finish(). Two accel_marker
;;; characters following each other produce a single literal accel_marker
;;; character.
;;;
;;; To feed markup to the parser, use g_markup_parse_context_parse() on the
;;; returned GMarkupParseContext. When done with feeding markup to the parser,
;;; use pango_markup_parser_finish() to get the data out of it, and then use
;;; g_markup_parse_context_free() to free it.
;;;
;;; This function is designed for applications that read pango markup from
;;; streams. To simply parse a string containing pango markup, the simpler
;;; pango_parse_markup() API is recommended instead.
;;;
;;; accel_marker :
;;;     character that precedes an accelerator, or 0 for none
;;;
;;; Returns :
;;;     a GMarkupParseContext that should be destroyed with
;;;     g_markup_parse_context_free().
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_markup_parser_finish ()
;;;
;;; gboolean
;;; pango_markup_parser_finish (GMarkupParseContext *context,
;;;                             PangoAttrList **attr_list,
;;;                             char **text,
;;;                             gunichar *accel_char,
;;;                             GError **error);
;;;
;;; After feeding a pango markup parser some data with
;;; g_markup_parse_context_parse(), use this function to get the list of pango
;;; attributes and text out of the markup. This function will not free context ,
;;; use g_markup_parse_context_free() to do so.
;;;
;;; context :
;;;     A valid parse context that was returned from pango_markup_parser_new()
;;;
;;; attr_list :
;;;     address of return location for a PangoAttrList, or NULL.
;;;
;;; text :
;;;     address of return location for text with tags stripped, or NULL.
;;;
;;; accel_char :
;;;     address of return location for accelerator char, or NULL.
;;;
;;; error :
;;;     address of return location for errors, or NULL
;;;
;;; Returns :
;;;     FALSE if error is set, otherwise TRUE
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.markup.lisp ------------------------------------------
