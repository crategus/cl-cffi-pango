;;; ----------------------------------------------------------------------------
;;; pango.layout.lisp
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
;;; Layout Objects
;;;
;;;     High-level layout driver objects
;;;
;;; Types and Values
;;;
;;;     PangoWrapMode
;;;     PangoEllipsizeMode
;;;     PangoAlignment
;;;     PangoLayoutLine
;;;     PangoLayoutRun
;;;
;;;     PangoLayout
;;;     PangoLayoutIter
;;;
;;; Functions
;;;
;;;     pango_layout_new
;;;     pango_layout_copy
;;;     pango_layout_get_context
;;;     pango_layout_context_changed
;;;     pango_layout_get_serial
;;;     pango_layout_set_text
;;;     pango_layout_get_text
;;;     pango_layout_get_character_count
;;;     pango_layout_set_markup
;;;     pango_layout_set_markup_with_accel
;;;     pango_layout_set_attributes
;;;     pango_layout_get_attributes
;;;     pango_layout_set_font_description
;;;     pango_layout_get_font_description
;;;     pango_layout_set_width
;;;     pango_layout_get_width
;;;     pango_layout_set_height
;;;     pango_layout_get_height
;;;     pango_layout_set_wrap
;;;     pango_layout_get_wrap
;;;     pango_layout_is_wrapped
;;;     pango_layout_set_ellipsize
;;;     pango_layout_get_ellipsize
;;;     pango_layout_is_ellipsized
;;;     pango_layout_set_indent
;;;     pango_layout_get_indent
;;;     pango_layout_get_spacing
;;;     pango_layout_set_spacing
;;;     pango_layout_set_line_spacing
;;;     pango_layout_get_line_spacing
;;;     pango_layout_set_justify
;;;     pango_layout_get_justify
;;;     pango_layout_set_auto_dir
;;;     pango_layout_get_auto_dir
;;;     pango_layout_get_direction
;;;     pango_layout_set_alignment
;;;     pango_layout_get_alignment
;;;     pango_layout_set_tabs
;;;     pango_layout_get_tabs
;;;     pango_layout_set_single_paragraph_mode
;;;     pango_layout_get_single_paragraph_mode
;;;     pango_layout_get_unknown_glyphs_count
;;;     pango_layout_get_log_attrs
;;;     pango_layout_get_log_attrs_readonly
;;;     pango_layout_index_to_pos
;;;     pango_layout_index_to_line_x
;;;     pango_layout_xy_to_index
;;;     pango_layout_get_cursor_pos
;;;     pango_layout_move_cursor_visually
;;;     pango_layout_get_extents
;;;     pango_layout_get_pixel_extents
;;;     pango_layout_get_size
;;;     pango_layout_get_pixel_size
;;;     pango_layout_get_baseline
;;;     pango_layout_get_line_count
;;;     pango_layout_get_line
;;;     pango_layout_get_line_readonly
;;;     pango_layout_get_lines
;;;     pango_layout_get_lines_readonly
;;;
;;;     pango_layout_get_iter
;;;     pango_layout_iter_copy
;;;     pango_layout_iter_free
;;;     pango_layout_iter_next_run
;;;     pango_layout_iter_next_char
;;;     pango_layout_iter_next_cluster
;;;     pango_layout_iter_next_line
;;;     pango_layout_iter_at_last_line
;;;     pango_layout_iter_get_index
;;;     pango_layout_iter_get_baseline
;;;     pango_layout_iter_get_run
;;;     pango_layout_iter_get_run_readonly
;;;     pango_layout_iter_get_line
;;;     pango_layout_iter_get_line_readonly
;;;     pango_layout_iter_get_layout
;;;     pango_layout_iter_get_char_extents
;;;     pango_layout_iter_get_cluster_extents
;;;     pango_layout_iter_get_run_extents
;;;     pango_layout_iter_get_line_yrange
;;;     pango_layout_iter_get_line_extents
;;;     pango_layout_iter_get_layout_extents
;;;
;;;     pango_layout_line_ref
;;;     pango_layout_line_unref
;;;     pango_layout_line_get_extents
;;;     pango_layout_line_get_height
;;;     pango_layout_line_get-length
;;;     pango_layout_line_get_pixel_extents
;;;     pango_layout_line_get-resolved-direction
;;;     pango_layout_line_get-start-index
;;;     pango_layout_line_get_x_ranges
;;;     pango_layout_line_index_to_x
;;;     pango_layout_line_is-paragraph-start
;;;     pango_layout_line_x_to_index
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ├── PangoLayoutIter
;;;     ╰── PangoLayoutLine
;;;
;;;     GEnum
;;;     ├── PangoAlignment
;;;     ├── PangoEllipsizeMode
;;;     ╰── PangoWrapMode
;;;
;;;     GObject
;;;     ╰── PangoLayout
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoWrapMode
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoWrapMode" wrap-mode
  (:export t
   :type-initializer "pango_wrap_mode_get_type")
  (:word 0)
  (:char 1)
  (:word-char 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'wrap-mode)
      "GEnum"
      (liber:symbol-documentation 'wrap-mode)
 "@version{2023-2-6}
  @begin{short}
    The @symbol{pango:wrap-mode} enumeration describes how to wrap the lines of
    a @class{pango:layout} object to the desired width.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"PangoWrapMode\" wrap-mode
  (:export t
   :type-initializer \"pango_wrap_mode_get_type\")
  (:word 0)
  (:char 1)
  (:word-char 2))
  @end{pre}
  @begin[code]{table}
    @entry[:word]{Wrap lines at word boundaries.}
    @entry[:char]{Wrap lines at character boundaries.}
    @entry[:word-char]{Wrap lines at word boundaries, but fall back to
      character boundaries if there is not enough space for a full word.}
  @end{table}
  @see-class{pango:layout}")

(export 'wrap-mode)

;;; ----------------------------------------------------------------------------
;;; enum PangoEllipsizeMode
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoEllipsizeMode" ellipsize-mode
  (:export t
   :type-initializer "pango_ellipsize_mode_get_type")
  (:none 0)
  (:start 1)
  (:middle 2)
  (:end 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'ellipsize-mode)
      "GEnum"
      (liber:symbol-documentation 'ellipsize-mode)
 "@version{2023-2-6}
  @begin{short}
    The @symbol{pango:ellipsize-mode} enumeration describes what sort of (if
    any) ellipsization should be applied to a line of text.
  @end{short}
  In the ellipsization process characters are removed from the text in order to
  make it fit to a given width and replaced with an ellipsis.
  @begin{pre}
(gobject:define-g-enum \"PangoEllipsizeMode\" ellipsize-mode
  (:export t
   :type-initializer \"pango_ellipsize_mode_get_type\")
  (:none 0)
  (:start 1)
  (:middle 2)
  (:end 3))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No ellipsization.}
    @entry[:start]{Omit characters at the start of the text.}
    @entry[:middle]{Omit characters in the middle of the text.}
    @entry[:end]{Omit characters at the end of the text.}
  @end{table}
  @see-class{pango:layout}")

(export 'ellipsize-mode)

;;; ----------------------------------------------------------------------------
;;; enum PangoAlignment
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoAlignment" alignment
  (:export t
   :type-initializer "pango_alignment_get_type")
  (:left 0)
  (:center 1)
  (:right 2))

#+liber-documentation
(setf (liber:alias-for-symbol 'alignment)
      "GEnum"
      (liber:symbol-documentation 'alignment)
 "@version{2023-2-6}
  @begin{short}
    The @symbol{pango:alignment} enumeration describes how to align the lines
    of a @class{pango:layout} object within the available space.
  @end{short}
  If the @class{pango:layout} object is set to justify using the
  @fun{pango:layout-justify} function, this only has effect for partial lines.
  @begin{pre}
(gobject:define-g-enum \"PangoAlignment\" alignment
  (:export t
   :type-initializer \"pango_alignment_get_type\")
  (:left 0)
  (:center 1)
  (:right 2))
  @end{pre}
  @begin[code]{table}
    @entry[:left]{Put all available space on the right.}
    @entry[:center]{Center the line within the available space.}
    @entry[:right]{Put all available space on the left.}
  @end{table}
  @see-class{pango:layout}
  @see-function{pango:layout-justify}")

;;; ----------------------------------------------------------------------------
;;; PangoLayoutRun
;;;
;;; typedef PangoGlyphItem PangoLayoutRun;
;;;
;;; The PangoLayoutRun structure represents a single run within a
;;; PangoLayoutLine; it is simply an alternate name for PangoGlyphItem. See the
;;; PangoGlyphItem docs for details on the fields.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoLayoutLine
;;; ----------------------------------------------------------------------------

;; TODO: This implementation does not work. There seems to be a problem, when
;; the C structure contains bitfields. For this case the implementation of
;; define-g-boxed-cstruct does not work.

#+nil
(gobject:define-g-boxed-cstruct layout-line "PangoLayoutLine"
  (:export t
   :type-initializer "pango_layout_line_get_type")
  (layout (g:object layout))
  (start-index :int)
  (length :int)
  (runs :pointer) ; (g:slist-t (g:boxed glyph-item))
  (is-paragraph-start :ushort) ; is bitfield : 1
  (resolved-dir :ushort)) ; is bitfield : 3

(glib:define-g-boxed-opaque layout-line "PangoLayoutLine"
  :export t
  :type-initializer "pango_layout_line_get_type"
  :alloc (error "PANGO:LAYOUT-LINE cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'layout-line)
      "GBoxed"
      (documentation 'layout-line 'type)
 "@version{2023-2-6}
  @begin{short}
    The @class{pango:layout-line} structure represents one of the lines
    resulting from laying out a paragraph via a @class{pango:layout} object.
  @end{short}
  The @class{pango:layout-line} instances are obtained by calling the
  @fun{pango:layout-line} function and are only valid until the text,
  attributes, or settings of the parent @class{pango:layout} object are
  modified. Routines for rendering @class{pango:layout} objects are provided in
  code specific to each rendering system.
  @begin{pre}
(gobject:define-g-boxed-cstruct layout-line \"PangoLayoutLine\"
  (:export t
   :type-initializer \"pango_layout_line_get_type\")
  (layout (g:object layout))
  (start-index :int)
  (length :int)
  (runs (g:slist-t (g:boxed glyph-item)))
  (is-paragraph-start :uint)
  (resolved-dir :uint))
  @end{pre}
  @begin[code]{table}
    @entry[layout]{The @class{pango:layout} object this line belongs to, might
      be @code{nil}.}
    @entry[start-index]{An integer with the start of line as byte index into
      the layout text.}
    @entry[length]{An integer with the length of line in bytes.}
    @entry[runs]{List of runs in the line, from left to right.}
    @entry[is-paragraph-start]{@em{True} if this is the first line of the
      paragraph.}
    @entry[resolved-dir]{Resolved @symbol{pango:direction} value of line.}
  @end{table}
  @see-class{pango:layout}
  @see-function{pango:layout-line}")

;;; ----------------------------------------------------------------------------
;;; PangoLayoutIter
;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque layout-iter "PangoLayoutIter"
  :export t
  :type-initializer "pango_layout_iter_get_type"
  :alloc (error "PANGO:LAYOUT-ITER cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'layout-iter)
      "GBoxed"
      (documentation 'layout-iter 'type)
 "@version{2023-2-11}
  @begin{short}
    A @class{pango:layout-iter} structure can be used to iterate over the
    visual extents of a @class{pango:layout} object.
  @end{short}
  The @class{pango:layout-iter} structure is opaque, and has no user visible
  fields.
  @see-constructor{pango:layout-iter}
  @see-constructor{pango:layout-iter-copy}
  @see-class{pango:layout}")

(export 'layout-iter)

;;; ----------------------------------------------------------------------------
;;; PangoLayout
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "PangoLayout" layout
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_layout_get_type")
  nil)

#+liber-documentation
(setf (documentation 'layout 'type)
 "@version{2023-2-6}
  @begin{short}
    The @class{pango:layout} class represents an entire paragraph of text.
  @end{short}
  It is initialized with a @class{pango:context} object, UTF-8 string and set
  of attributes for that string. Once that is done, the set of formatted lines
  can be extracted from the object, the layout can be rendered, and conversion
  between logical character positions within the layout's text, and the
  physical position of the resulting glyphs can be made.

  There are also a number of parameters to adjust the formatting of a
  @class{pango:layout} object. It is possible, as well, to ignore the 2-D setup,
  and simply treat the results of a @class{pango:layout} object as a list of
  lines.
  @image[layout]{Figure. Adjustable parameters (on the left) and font metrics
    (on the right) for a Pango layout}

  The @class{pango:layout} class is opaque, and has no user visible fields.
  @see-constructor{pango:layout-new}
  @see-constructor{pango:layout-copy}
  @see-class{pango:context}")

;;; ----------------------------------------------------------------------------
;;; pango_layout_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_new" layout-new) (g:object layout)
 #+liber-documentation
 "@version{2023-2-6}
  @argument[context]{a @class{pango:context} object}
  @return{The newly allocated @class{pango:layout} object.}
  @begin{short}
    Create a new Pango layout with attributes initialized to default values for
    a particular @class{pango:context} object.
  @end{short}
  @see-class{pango:layout}
  @see-class{pango:context}"
  (context (g:object context)))

(export 'layout-new)

;;; ----------------------------------------------------------------------------
;;; pango_layout_copy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_copy" layout-copy) (g:object layout)
 #+liber-documentation
 "@version{2023-2-6}
  @argument[src]{a @class{pango:layout} object}
  @begin{return}
    The newly allocated @class{pango:layout} object.
  @end{return}
  @begin{short}
    Does a deep copy-by-value of the @arg{src} layout.
  @end{short}
  The attribute list, tab array, and text from the original layout are all
  copied by value.
  @see-class{pango:layout}"
  (src (g:object layout)))

(export 'layout-copy)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_context ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_context" layout-context) (g:object context)
 #+liber-documentation
 "@version{2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    The @class{pango:context} object for the layout.
  @end{return}
  @begin{short}
    Retrieves the Pango context used for this layout.
  @end{short}
  @see-class{pango:layout}
  @see-class{pango:context}"
  (layout (g:object layout)))

(export 'layout-context)

;;; ----------------------------------------------------------------------------
;;; pango_layout_context_changed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_context_changed" layout-context-changed) :void
 #+liber-documentation
 "@version{2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @begin{short}
    Forces recomputation of any state in the Pango layout that might depend
    on the context of @arg{layout}.
  @end{short}
  This function should be called if you make changes to the context subsequent
  to creating the Pango layout.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-context-changed)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_serial ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_serial" layout-serial) :uint
 #+liber-documentation
 "@version{2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    An unsigned integer with the current serial number of @arg{layout}.
  @end{return}
  @begin{short}
    Returns the current serial number of @arg{layout}.
  @end{short}
  The serial number is initialized to an small number larger than zero when a
  new layout is created and is increased whenever the layout is changed using
  any of the setter functions, or the @class{pango:context} object it uses has
  changed. The serial may wrap, but will never have the value 0. Since it can
  wrap, never compare it with \"less than\", always use \"not equals\".

  This can be used to automatically detect changes to a Pango layout, and is
  useful for example to decide whether a layout needs redrawing. To force the
  serial to be increased, use the @fun{pango:layout-context-changed} function.
  @see-class{pango:layout}
  @see-class{pango:context}
  @see-function{pango:layout-context-changed}"
  (layout (g:object layout)))

(export 'layout-serial)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_text ()
;;; pango_layout_get_text ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-text) (text layout)
  (cffi:foreign-funcall "pango_layout_set_text"
                        (g:object layout) layout
                        (:string :free-to-foreign nil) text
                        :int -1)
  text)

(cffi:defcfun ("pango_layout_get_text" layout-text) :string
 #+liber-documentation
 "@version{2023-2-6}
  @syntax[]{(pango:layout-text layout) => text}
  @syntax[]{(setf (pango:layout-text layout) text)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[text]{a valid UTF-8 string}
  @begin{short}
    Accessor of the text of a @class{pango:layout} object.
  @end{short}
  The @fun{pango:layout-text} function gets the text in the Pango layout. The
  @setf{pango:layout-text} function sets the text of the Pango layout.

  Note that if you have used the @fun{pango:layout-set-markup} or
  @fun{pango:layout-set-markup-with-accel} functions on the Pango layout before,
  you may want to call the @fun{pango:layout-attributes} function to clear the
  attributes set on the layout from the markup as this function does not clear
  attributes.
  @see-class{pango:layout}
  @see-function{pango:layout-set-markup}
  @see-function{pango:layout-set-markup-with-accel}
  @see-function{pango:layout-attributes}"
  (layout (g:object layout)))

(export 'layout-text)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_character_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_character_count" layout-character-count) :int
 #+liber-documentation
 "@version{2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    An integer with the number of Unicode characters in the text of
    @arg{layout}.
  @end{return}
  @begin{short}
    Returns the number of Unicode characters in the the text of @arg{layout}.
  @end{short}
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-character-count)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_markup ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_set_markup" %layout-set-markup) :void
  (layout (g:object layout))
  (markup :string)
  (length :int))

(defun layout-set-markup (layout markup)
 #+liber-documentation
 "@version{2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @argument[markup]{a string with the marked-up text}
  @begin{short}
    Same as the @fun{pango:layout-set-markup-with-accel} function, but the
    markup text is not scanned for accelerators.
  @end{short}
  @see-class{pango:layout}
  @see-function{pango:layout-set-markup-with-accel}"
  (%layout-set-markup layout markup -1))

(export 'layout-set-markup)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_markup_with_accel ()
;;; ----------------------------------------------------------------------------

;; TODO: Replace :uint32 with the UNICHAR type! This is defined elsewhere.

(cffi:defcfun ("pango_layout_set_markup_with_accel"
                %layout-set-markup-with-accel) :void
  (layout (g:object layout))
  (markup :string)
  (length :int)
  (marker g:unichar)
  (char (:pointer :uint32)))

(defun layout-set-markup-with-accel (layout markup marker)
 #+liber-documentation
 "@version{2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @argument[markup]{a string with the marked-up text}
  @argument[marker]{an unsigned integer with the marker for accelerators
    in the text}
  @begin{return}
    Returns the first located accelerator.
  @end{return}
  @begin{short}
    Sets the layout text and attribute list from marked-up text (see markup
    format).
  @end{short}
  Replaces the current text and attribute list.

  If @arg{marker} is nonzero, the given character will mark the character
  following it as an accelerator. For example, @arg{marker} might be an
  ampersand or underscore. All characters marked as an accelerator will receive
  a @code{PANGO_UNDERLINE_LOW} attribute, and the first character so marked
  will be returned. Two @arg{marker} characters following each other
  produce a single literal @arg{marker} character.
  @see-class{pango:layout}"
  (cffi:with-foreign-object (char :uint32)
    (%layout-set-markup-with-accel layout markup -1 marker char)
    (values (cffi:mem-ref char 'g:unichar))))

(export 'layout-set-markup-with-accel)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_attributes ()
;;; pango_layout_set_attributes ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-attributes) (attrs layout)
  (cffi:foreign-funcall "pango_layout_set_attributes"
                        (g:object layout) layout
                        (g:boxed attr-list) attrs
                        :void)
  attrs)

(cffi:defcfun ("pango_layout_get_attributes" layout-attributes)
    (g:boxed attr-list)
 #+liber-documentation
 "@version{2023-2-6}
  @syntax[]{(pango:layout-attributes layout) => attrs}
  @syntax[]{(setf (pango:layout-attributes layout) attrs)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[attrs]{a @class{pango:attr-list} instance, can be @code{nil}}
  @begin{short}
    Accessor of the attribute list for the layout.
  @end{short}
  The @fun{pango:layout-attributes} function gets the attribute list for the
  layout, if any. The @setf{pango:layout-attributes} function sets the text
  attributes for a layout object.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-attributes)

;;; ----------------------------------------------------------------------------
;;; pango_layout_set_font_description ()
;;; pango_layout_get_font_description ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-font-description) (desc layout)
  (cffi:foreign-funcall "pango_layout_set_font_description"
                        (g:object layout) layout
                        (g:boxed font-description) desc
                        :void)
  desc)

(cffi:defcfun ("pango_layout_get_font_description" layout-font-description)
    (g:boxed font-description)
 #+liber-documentation
 "@version{2023-2-6}
  @syntax[]{(pango:layout-font-description layout) => desc}
  @syntax[]{(setf (pango:layout-font-description layout) desc)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[desc]{a @class{pango:font-description} instance, or @code{nil} to
    unset the current font description}
  @begin{short}
    Accessor of the font description of the Pango layout.
  @end{short}
  The @fun{pango:layout-font-description} function gets the font description for
  the Pango layout. The @setf{pango:layout-font-description} function sets the
  default font description for the Pango layout.

  If no font description is set on the layout, the font description from the
  layout's context is used.
  @see-class{pango:layout}
  @see-class{pango:font-description}"
  (layout (g:object layout)))

(export 'layout-font-description)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_width ()
;;; pango_layout_set_width ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-width) (width layout)
  (cffi:foreign-funcall "pango_layout_set_width"
                        (g:object layout) layout
                        :int width
                        :void)
  width)

(cffi:defcfun ("pango_layout_get_width" layout-width) :int
 #+liber-documentation
 "@version{2023-2-6}
  @syntax[]{(pango:layout-width layout) => width}
  @syntax[]{(setf (pango:layout-width layout) width)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[width]{an integer with the desired width in Pango units, or -1 to
    indicate that no wrapping or ellipsization should be performed}
  @begin{short}
    Accessor of the width in Pango units of a Pango layout.
  @end{short}
  The @fun{pango:layout-width} function gets the width to which the lines of
  the Pango layout should wrap. The @setf{pango:layout-width} function sets the
  width to which the lines of the Pango layout should wrap or ellipsized. The
  default value is -1: no width set.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-width)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_height ()
;;; pango_layout_set_height ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-height) (height layout)
  (cffi:foreign-funcall "pango_layout_set_height"
                        (g:object layout) layout
                        :int height
                        :void)
  height)

(cffi:defcfun ("pango_layout_get_height" layout-height) :int
 #+liber-documentation
 "@version{2023-2-6}
  @syntax[]{(pango:layout-height layout) => height}
  @syntax[]{(setf (pango:layout-height layout) height)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[height]{an integer with the desired height of the layout in Pango
    units if positive, or desired number of lines if negative}
  @begin{short}
    Accessor of the height in Pango units of the Pango layout.
  @end{short}
  The @fun{pango:layout-height} function gets the height of @arg{layout} used
  for ellipsization. The @setf{pango:layout-height} function sets the height to
  which the Pango layout should be ellipsized at. There are two different
  behaviors, based on whether @arg{height} is positive or negative.

  If @arg{height} is positive, it will be the maximum height of the layout.
  Only lines would be shown that would fit, and if there is any text omitted,
  an ellipsis added. At least one line is included in each paragraph regardless
  of how small the height value is. A value of zero will render exactly one
  line for the entire layout.

  If @arg{height} is negative, it will be the (negative of) maximum number of
  lines per paragraph. That is, the total number of lines shown may well be
  more than this value if the layout contains multiple paragraphs of text. The
  default value of -1 means that first line of each paragraph is ellipsized.
  This behvaior may be changed in the future to act per layout instead of per
  paragraph. File a bug against pango at http://bugzilla.gnome.org/ if your
  code relies on this behavior.

  Height setting only has effect if a positive width is set on layout and
  ellipsization mode of layout is not @code{:none}. The behavior is undefined
  if a height other than -1 is set and ellipsization mode is set to
  @code{:none}, and may change in the future.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-height)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_wrap ()
;;; pango_layout_set_wrap ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-wrap) (wrap layout)
  (cffi:foreign-funcall "pango_layout_set_wrap"
                        (g:object layout) layout
                        wrap-mode wrap
                        :void)
  wrap)

(cffi:defcfun ("pango_layout_get_wrap" layout-wrap) wrap-mode
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-wrap layout) => wrap}
  @syntax[]{(setf (pango:layout-wrap layout) wrap)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[wrap]{a value of the @symbol{pango:wrap-mode} enumeration}
  @begin{short}
    Accessor of the wrap mode of the Pango layout.
  @end{short}
  The @fun{pango:layout-wrap} function gets the wrap mode for the layout. The
  @setf{pango:layout-wrap} function sets the wrap mode. The wrap mode only has
  effect if a width is set on the layout with the @fun{pango:layout-width}
  function. To turn off wrapping, set the width to -1.

  Use the @fun{pango:layout-is-wrapped} function to query whether any
  paragraphs were actually wrapped.
  @see-class{pango:layout}
  @see-function{pango:layout-width}
  @see-function{pango:layout-is-wrapped}"
  (layout (g:object layout)))

(export 'layout-wrap)

;;; ----------------------------------------------------------------------------
;;; pango_layout_is_wrapped ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_is_wrapped" layout-is-wrapped) :boolean
 #+liber-documentation
 "@version{#2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    @em{True} if any paragraphs had to be wrapped, @em{false} otherwise.
  @end{return}
  @begin{short}
    Queries whether the layout had to wrap any paragraphs.
  @end{short}
  This returns @em{true} if a positive width is set on the layout, ellipsization
  mode of the layout is set to @code{:none}, and there are paragraphs exceeding
  the layout width that have to be wrapped.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-is-wrapped)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_ellipsize ()
;;; pango_layout_set_ellipsize ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-ellipsize) (ellipsize layout)
  (cffi:foreign-funcall "pango_layout_set_ellipsize"
                        (g:object layout) layout
                        ellipsize-mode ellipsize
                        :void)
  ellipsize)

(cffi:defcfun ("pango_layout_get_ellipsize" layout-ellipsize) ellipsize-mode
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-ellipsize layout) => ellipsize}
  @syntax[]{(setf (pango:layout-ellipsize layout) ellipsize)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[ellipsize]{a value of the @symbol{pango:ellipsize-mode} enumeration}
  @begin{short}
    Accessor of the ellipsization mode for the layout.
  @end{short}
  The @fun{pango:layout-ellipsize} function gets the type of ellipsization being
  performed for the layout. The @setf{pango:layout-ellipsize} function sets the
  type of ellipsization being performed for the layout. Depending on the
  ellipsization mode ellipsize text is removed from the start, middle, or end
  of text so they fit within the width and height of layout set with the
  @fun{pango:layout-width} and @fun{pango:layout-height} functions.

  If the layout contains characters such as newlines that force it to be layed
  out in multiple paragraphs, then whether each paragraph is ellipsized
  separately or the entire layout is ellipsized as a whole depends on the set
  height of the layout. See the @fun{pango:layout-height} function for details.

  Use the @fun{pango:layout-is-ellipsized} function to query whether any
  paragraphs were actually ellipsized.
  @see-class{pango:layout}
  @see-function{pango:layout-width}
  @see-function{pango:layout-height}
  @see-function{pango:layout-is-ellipsized}"
  (layout (g:object layout)))

(export 'layout-ellipsize)

;;; ----------------------------------------------------------------------------
;;; pango_layout_is_ellipsized ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_is_ellipsized" layout-is-ellipsized) :boolean
 #+liber-documentation
 "@version{#2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    @em{True} if any paragraphs had to be ellipsized, @em{false} otherwise.
  @end{return}
  @begin{short}
    Queries whether the layout had to ellipsize any paragraphs.
  @end{short}
  This returns @em{true} if the ellipsization mode for the layout is not
  @code{:none}, a positive width is set on the layout, and there are paragraphs
  exceeding that width that have to be ellipsized.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-is-ellipsized)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_indent ()
;;; pango_layout_set_indent ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-indent) (indent layout)
  (cffi:foreign-funcall "pango_layout_set_indent"
                        (g:object layout) layout
                        :int indent
                        :void)
  indent)

(cffi:defcfun ("pango_layout_get_ident" layout-indent) :int
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-indent layout) => indent}
  @syntax[]{(setf (pango:layout-indent layout) indent)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[indent]{an integer with the amount by which to indent}
  @begin{short}
    Accessor of the indent in Pango units of the Pango layout.
  @end{short}
  The @fun{pango:layout-indent} function gets the paragraph indent width in
  Pango units. A negative value indicates a hanging indentation. The
  @setf{pango:layout-indent} function sets the width in Pango units to indent
  each paragraph. A negative value of indent will produce a hanging indentation.
  That is, the first line will have the full width, and subsequent lines will
  be indented by the absolute value of indent.

  The indent setting is ignored if the layout alignment is set to the
  @code{:center} value.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-indent)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_spacing ()
;;; pango_layout_set_spacing ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-spacing) (spacing layout)
  (cffi:foreign-funcall "pango_layout_set_spacing"
                        (g:object layout) layout
                        :int spacing
                        :void)
  spacing)

(cffi:defcfun ("pango_layout_get_spacing" layout-spacing) :int
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-spacing layout) => spacing}
  @syntax[]{(setf (pango:layout-spacing layout) spacing)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[spacing]{an integer with the amount of spacing}
  @begin{short}
    Accessor of the spacing in Pango units of the Pango layout.
  @end{short}
  The @fun{pango:layout-spacing} function gets the amount of spacing in Pango
  units between the lines of the layout. The @setf{pango:layout-spacing}
  function sets the amount of spacing. When placing lines with spacing, Pango
  arranges things so that
  @begin{pre}
line2.top = line1.bottom + spacing
  @end{pre}
  @begin[Note]{dictionary}
    Since 1.44, Pango defaults to using the line height (as determined by the
    font) for placing lines. The spacing set with this function is only taken
    into account when the line-height factor is set to zero with the
    @fun{pango:layout-line-spacing} function.
  @end{dictionary}
  @see-class{pango:layout-spacing}
  @see-function{pango:layout-line-spacing}"
  (layout (g:object layout)))

(export 'layout-spacing)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line_spacing ()
;;; pango_layout_set_line_spacing ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defun (setf layout-line-spacing) (factor layout)
  (let ((factor (coerce factor 'single-float)))
    (cffi:foreign-funcall "pango_layout_set_line_spacing"
                          (g:object layout) layout
                          :float factor
                          :void)
    factor))

#+pango-1-44
(cffi:defcfun ("pango_layout_get_line_spacing" layout-line-spacing) :float
 #+liber-documentation
 "@version{2023-12-3}
  @syntax[]{(pango:layout-line-spacing layout) => factor}
  @syntax[]{(setf (pango:layout-line-spacing layout) factor)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[factor]{a number coerced to a float with the new line spacing
    factor}
  @begin{short}
    Accessor of the line spacing for the Pango layout.
  @end{short}
  The @fun{pango:layout-line-spacing} function gets the value of the line
  spacing. The @setf{pango:layout-line-spacing} function sets a factor for line
  spacing. Typical values are: 0, 1, 1.5, 2. The default values is 0.

  If the @arg{factor} argument is non-zero, lines are placed so that
  @begin{pre}
baseline2 = baseline1 + factor * height2
  @end{pre}
  where @code{height2} is the line height of the second line as determined by
  the font. In this case, the spacing set with the @fun{pango:layout-spacing}
  function is ignored.

  If the @arg{factor} argument is zero, spacing is applied as before.

  Since 1.44
  @see-class{pango:layout}
  @see-function{pango:layout-spacing}"
  (layout (g:object layout)))

#+pango-1-44
(export 'layout-line-spacing)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_justify ()
;;; pango_layout_set_justify ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-justify) (justify layout)
  (cffi:foreign-funcall "pango_layout_set_justify"
                        (g:object layout) layout
                        :boolean justify
                        :void)
  justify)

(cffi:defcfun ("pango_layout_get_justify" layout-justify) :boolean
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-justify layout) => justify}
  @syntax[]{(setf (pango:layout-justify layout) justify)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[justify]{a boolean whether the lines in the layout should be
    justified}
  @begin{short}
    Accessor of the justify value of the Pango layout.
  @end{short}
  The @fun{pango:layout-justify} function gets whether each complete line should
  be stretched to fill the entire width of the layout. The
  @setf{pango:layout-justify} function sets the justify. This stretching is
  typically done by adding whitespace, but for some scripts (such as Arabic),
  the justification may be done in more complex ways, like extending the
  characters.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-justify)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_auto_dir ()
;;; pango_layout_set_auto_dir ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-auto-dir) (auto-dir layout)
  (cffi:foreign-funcall "pango_layout_set_auto_dir"
                        (g:object layout) layout
                        :boolean auto-dir
                        :void)
  auto-dir)

(cffi:defcfun ("pango_layout_get_auto_dir" layout-auto-dir) :boolean
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-auto-dir layout) => auto-dir}
  @syntax[]{(setf (pango:layout-auto-dir layout) auto-dir)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[auto-dir]{if @em{true}, compute the bidirectional base direction
    from the layout's contents}
  @begin{short}
    The @fun{pango:layout-auto-dir} function gets whether to calculate the
    bidirectional base direction for the layout according to the contents of
    the layout.
  @end{short}
  The @setf{pango:layout-auto-dir} function sets whether to calculate the
  bidirectional base direction for the layout according to the contents of the
  layout. When this flag is on (the default), then paragraphs in layout that
  begin with strong right-to-left characters (Arabic and Hebrew principally),
  will have right-to-left layout, paragraphs with letters from other scripts
  will have left-to-right layout. Paragraphs with only neutral characters get
  their direction from the surrounding paragraphs.

  When @em{false}, the choice between left-to-right and right-to-left layout is
  done according to the base direction of the layout's Pango context. (See the
  @fun{pango:context-base-dir} function).

  When the auto-computed direction of a paragraph differs from the base
  direction of the context, the interpretation of @code{:left} and
  @code{:right} are swapped.
  @see-class{pango:layout}
  @see-function{pango:context-base-dir}"
  (layout (g:object layout)))

(export 'layout-auto-dir)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_direction ()
;;; ----------------------------------------------------------------------------

#+pango-1-46
(cffi:defcfun ("pango_layout_get_direction" layout-direction) direction
 #+liber-documentation
 "@version{#2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @argument[index]{an integer with the byte index of the char}
  @return{The @symbol{pango:direction} value for the text direction at
    @arg{index}.}
  @begin{short}
    Gets the text direction at the given character position in the layout.
  @end{short}

  Since 1.46
  @see-class{pango:layout}
  @see-symbol{pango:direction}"
  (layout (g:object layout))
  (index :int))

#+pango-1-46
(export 'layout-direction)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_alignment ()
;;; pango_layout_set_alignment ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-alignment) (alignment layout)
  (cffi:foreign-funcall "pango_layout_set_alignment"
                        (g:object layout) layout
                        alignment alignment
                        :void)
  alignment)

(cffi:defcfun ("pango_layout_get_alignment" layout-alignment) alignment
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-alignment layout) => alignment}
  @syntax[]{(setf (pango:layout-alignment layout) alignment)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[alignment]{a @symbol{pango:alignment} value}
  @begin{short}
    Accessor of the alignement of the layout.
  @end{short}
  The @fun{pango:layout-alignment} function gets the alignment for the layout:
  how partial lines are positioned within the horizontal space available. The
  @setf{pango:layout-alignment} function sets the alignment.
  @see-class{pango:layout}
  @see-symbol{pango:alignment}"
  (layout (g:object layout)))

(export 'layout-alignment)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_tabs ()
;;; pango_layout_set_tabs ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-tabs) (tabs layout)
  (cffi:foreign-funcall "pango_layout_set_tabs"
                        (g:object layout) layout
                        (g:boxed tab-array) tabs
                        :void)
  tabs)

(cffi:defcfun ("pango_layout_get_tabs" layout-tabs) (g:boxed tab-array :return)
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-tabs layout) => tabs}
  @syntax[]{(setf (pango:layout-tabs layout) tabs)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[tabs]{a @class{pango:tab-array} instance, or @code{nil}}
  @begin{short}
    Accessor of the tabs for the Pango layout.
  @end{short}
  The @fun{pango:layout-tabs} function gets the current @symbol{pango:tab-array}
  instance used by this layout. The @setf{pango:layout-tabs} function sets the
  tabs to use for the layout.

  If no @class{pango:tab-array} instance has been set, then the default tabs
  are in use and @code{nil} is returned. Default tabs are every 8 spaces.
  @see-class{pango:layout}
  @see-class{pango:tab-array}"
  (layout (g:object layout)))

(export 'layout-tabs)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_single_paragraph_mode ()
;;; pango_layout_set_single_paragraph_mode ()
;;; ----------------------------------------------------------------------------

(defun (setf layout-single-paragraph-mode) (setting layout)
  (cffi:foreign-funcall "layout-single-paragraph-mode"
                        (g:object layout) layout
                        :boolean setting
                        :void)
  setting)

(cffi:defcfun ("pango_layout_get_single_paragraph_mode"
                layout-single-paragraph-mode) :boolean
 #+liber-documentation
 "@version{#2023-2-6}
  @syntax[]{(pango:layout-tabs layout) => tabs}
  @syntax[]{(setf (pango:layout-tabs layout) tabs)}
  @argument[layout]{a @class{pango:layout} object}
  @argument[setting]{whether the layout does not break paragraphs at paragraph
    separator characters}
  @begin{short}
    Accessor of the single paragraph mode of the Pango Layout.
  @end{short}
  If @arg{setting} is @em{true}, do not treat newlines and similar characters
  as paragraph separators. Instead, keep all text in a single paragraph, and
  display a glyph for paragraph separator characters. Used when you want to
  allow editing of newlines on a single text line.
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-single-paragraph-mode)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_unknown_glyphs_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_unknown_glyphs_count"
                layout-unknown-glyphs-count) :int
 #+liber-documentation
 "@version{#2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @return{The integer with the number of unknown glyphs in @arg{layout}.}
  @begin{short}
    Counts the number unknown glyphs in layout.
  @end{short}
  That is, zero if glyphs for all characters in the layout text were found, or
  more than zero otherwise.

  This function can be used to determine if there are any fonts available to
  render all characters in a certain string, or when used in combination with
  the @code{:fallback} value of the @symbol{pango:attr-type} enumeration, to
  check if a certain font supports all the characters in the string.
  @see-class{pango:layout}
  @see-symbol{pango:attr-type}"
  (layout (g:object layout)))

(export 'layout-unknown-glyphs-count)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_log_attrs ()                          not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation

(cffi:defcfun ("pango_layout_get_log_attrs" %layout-log-attrs) :void
  (layout (g:object layout))
  (attrs (:pointer (:struct log-attr)))
  (n-attrs (:pointer :int)))

(defun layout-log-attrs (layout)
 #+liber-documentation
 "@version{#2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    A list of logical attributes of type @symbol{pango:log-attr}.
  @end{return}
  @begin{short}
    Retrieves a list of logical attributes for each character in the layout.
  @end{short}
  @see-class{pango:layout}
  @see-symbol{pango:log-attr}"
  (cffi:with-foreign-objects ((attrs-ptr :pointer) (n-attrs :int))
    (%layout-log-attrs layout attrs-ptr n-attrs)
    (iter (with attrs-ar = (cffi:mem-ref attrs-ptr :pointer))
          (for i from 0 below (cffi:mem-ref n-attrs :int))
          (collect (cffi:mem-aref attrs-ar :pointer i))
          (finally (glib:free attrs-ptr)))))

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_log_attrs_readonly ()                 not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation

(cffi:defcfun ("pango_layout_get_log_attrs_readonly"
                %layout-log-attrs-readonly) (:pointer (:struct log-attr))
  (layout (g:object layout))
  (n-attrs (:pointer :int)))

(defun layout-log-attrs-readonly (layout)
 #+liber-documentation
 "@version{#2023-2-6}
  @argument[layout]{a @class{pango:layout} object}
  @return{The list of logical attributes of type @symbol{pango:log-attr}.}
  @begin{short}
    Retrieves a list of logical attributes for each character in the layout.
  @end{short}
  This is a faster alternative to the @fun{pango:layout-log-attrs} function. The
  returned list is part of @arg{layout} and must not be modified. Modifying the
  layout will invalidate the returned list.
  @see-class{pango:layout}
  @see-function{pango:layout-log-attrs}"
  (cffi:with-foreign-object (n-attrs :int)
    (let ((attrs-ptr (%layout-log-attrs-readonly layout n-attrs)))
      (iter (for count from 0 below (cffi:mem-ref n-attrs :int))
            (collect (cffi:mem-ref attrs-ptr '(:struct log-attr) count))
            (finally (glib:free attrs-ptr))))))

;;; ----------------------------------------------------------------------------
;;; pango_layout_index_to_pos ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_index_to_pos" %layout-index-to-pos) :void
  (layout (g:object layout))
  (index :int)
  (pos (:pointer (:struct rectangle))))

(defun layout-index-to-pos (layout index)
 #+liber-documentation
 "@version{2023-2-7}
  @syntax[]{(pango:layout-index-to-pos layout index) => x, y, width, height}
  @argument[layout]{a @class{pango:layout} object}
  @argument[index]{an integer with the byte index within @arg{layout}}
  @argument[x]{an integer with the x coordinate}
  @argument[y]{an integer with the y coordinate}
  @argument[width]{an integer with the width}
  @argument[height]{an integer with the height}
  @begin{short}
    Converts from an index within a Pango layout to the onscreen position
    corresponding to the grapheme at that index, which is represented as
    the coordinates of a rectangle.
  @end{short}

  Note that @arg{x} is always the leading edge of the grapheme and
  @code{@arg{x} + @arg{width}} the trailing edge of the grapheme. If the
  directionality of the grapheme is right-to-left, then @arg{witdh} will be
  negative.
  @see-class{pango:layout}"
  (cffi:with-foreign-object (pos '(:struct rectangle))
    (%layout-index-to-pos layout index pos)
    (cffi:with-foreign-slots ((x y width height) pos (:struct rectangle))
      (values x y width height))))

(export 'layout-index-to-pos)

;;; ----------------------------------------------------------------------------
;;; pango_layout_index_to_line_x ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_index_to_line_x" %layout-index-to-line-x) :void
  (layout (g:object layout))
  (index :int)
  (trailing :boolean)
  (line (:pointer :int))
  (xpos (:pointer :int)))

(defun layout-index-to-line-x (layout index trailing)
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @argument[index]{an integer with the byte index of a grapheme within the
    layout}
  @argument[trailing]{an integer indicating the edge of the grapheme to
    retrieve the position of, if 0, the trailing edge of the grapheme, if > 0,
    the leading of the grapheme}
  @begin{return}
    @code{line} -- an integer with the resulting line index, which will between
      0 and @code{(pango:layout-line-count layout) - 1}, or @code{nil} @br{}
    @code{xpos} -- an integer with the resulting position within line
      in Pango units per device unit, or @code{nil}
  @end{return}
  @begin{short}
    Converts from byte index within the layout to @arg{line} and @arg{xpos}
    position.
  @end{short}
  The @arg{xpos} position is measured from the left edge of the line.
  @see-class{pango:layout}"
  (cffi:with-foreign-objects ((line :int) (xpos :int))
    (%layout-index-to-line-x layout index trailing line xpos)
    (values (cffi:mem-ref line :int)
            (cffi:mem-ref xpos :int))))

(export 'layout-index-to-line-x)

;;; ----------------------------------------------------------------------------
;;; pango_layout_xy_to_index ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_xy_to_index" %layout-xy-to-index) :boolean
  (layout (g:object layout))
  (x :int)
  (y :int)
  (index (:pointer :int))
  (trailing (:pointer :int)))

(defun layout-xy-to-index (layout x y)
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @argument[x]{an integer x offse in Pango units from the left edge of the
    layout}
  @argument[y]{an integer y offset in Pango units from the top edge of the
    layout}
  @begin{return}
    @code{index} -- an integer with the calculated byte index @br{}
    @code{trailing} -- an integer indicating where in the grapheme the user
      clicked, it will either be zero, or the number of characters in the
      grapheme, 0 represents the trailing edge of the grapheme
  @end{return}
  @begin{short}
    Converts from @arg{x} and @arg{y} position within a layout to the byte
    index to the character at that logical position.
  @end{short}
  If the @arg{y} position is not inside the layout, the closest position is
  chosen (the position will be clamped inside the layout). If the @arg{x}
  position is not within the layout, then the start or the end of the line is
  chosen as described for the @fun{pango:layout-line-x-to-index} function. If
  either the @arg{x} or @arg{y} positions were not inside the layout, then the
  function returns @code{nil}.
  @see-class{pango:layout}
  @see-function{pango:layout-line-x-to-index}"
  (cffi:with-foreign-objects ((index :int) (trailing :int))
    (when (%layout-xy-to-index layout x y index trailing)
      (values (cffi:mem-ref index :int)
              (cffi:mem-ref trailing :int)))))

(export 'layout-xy-to-index)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_cursor_pos ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_cursor_pos" %layout-cursor-pos) :void
  (layout (g:object layout))
  (index :int)
  (strong-pos (:pointer (:struct rectangle)))
  (weak-pos (:pointer (:struct rectangle))))

(defun layout-cursor-pos (layout index)
 #+liber-documentation
 "@version{2023-2-7}
  @syntax[]{(pango:layout-cursor-pos layout index) => strong, weak}
  @argument[layout]{a @class{pango:layout} object}
  @argument[index]{an integer with the byte index of the cursor}
  @argument[strong]{a list with the @arg{x}, @arg{y}, @arg{width}, @arg{height}
    values of the rectangle with the strong cursor position}
  @argument[weak]{a list with the @arg{x}, @arg{y}, @arg{width}, @arg{height}
    values of the rectangle with the weak cursor position}
  @begin{short}
    Given an index within a layout, determines the positions that of the strong
    and weak cursors if the insertion point is at that index.
  @end{short}
  The position of each cursor is stored as a zero-width rectangle. The strong
  cursor location is the location where characters of the directionality equal
  to the base direction of the layout are inserted. The weak cursor location is
  the location where characters of the directionality opposite to the base
  direction of the layout are inserted.
  @see-class{pango:layout}"
  (cffi:with-foreign-objects ((strong '(:struct rectangle))
                              (weak '(:struct rectangle)))
    (%layout-cursor-pos layout index strong weak)
    (let (values1 values2)
      (cffi:with-foreign-slots ((x y width height) strong (:struct rectangle))
        (setf values1 (list x y width height)))
      (cffi:with-foreign-slots ((x y width height) weak (:struct rectangle))
        (setf values2 (list x y width height)))
      (values values1 values2))))

(export 'layout-cursor-pos)

;;; ----------------------------------------------------------------------------
;;; pango_layout_move_cursor_visually ()
;;; ----------------------------------------------------------------------------

;; TODO: Rework the documentation

(cffi:defcfun ("pango_layout_move_cursor_visually" %layout-move-cursor-visually)
    :void
  (layout (g:object layout))
  (strong :boolean)
  (index :int)
  (trailing :int)
  (direction :int)
  (new-index (:pointer :int))
  (new-traling (:pointer :int)))

(defun layout-move-cursor-visually (layout strong index trailing direction)
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @argument[strong]{a boolean whether the moving cursor is the strong cursor or
    the weak cursor, the strong cursor is the cursor corresponding to text
    insertion in the base direction for the layout}
  @argument[index]{an integer with the byte index of the grapheme for the
    old index}
  @argument[trailing]{an integer, if 0, the cursor was at the trailing edge
    of the grapheme indicated by @arg{index}, if > 0, the cursor was at the
    leading edge}
  @argument[direction]{an integer with the direction to move cursor, a negative
    value indicates motion to the left}
  @begin{return}
    @code{new-index} -- an integer with the new cursor byte index, a value of
      -1 indicates that the cursor has been moved off the beginning of the
      layout, a value of @code{G_MAXINT} indicates that the cursor has been
      moved off the end of the layout @br{}
    @code{new-trailing} --an integer with the number of characters to move
      forward from the location returned for @arg{new-index} to get the position
      where the cursor should be displayed, this allows distinguishing the
      position at the beginning of one line from the position at the end of the
      preceding line, @arg{new-index} is always on the line where the cursor
      should be displayed
  @end{return}
  @begin{short}
    Computes a new cursor position from an old position and a count of positions
    to move visually.
  @end{short}
  If @arg{direction} is positive, then the new strong cursor position will be
  one position to the right of the old cursor position. If @arg{direction} is
  negative, then the new strong cursor position will be one position to the
  left of the old cursor position.

  In the presence of bidirectional text, the correspondence between logical
  and visual order will depend on the direction of the current run, and there
  may be jumps when the cursor is moved off of the end of a run.

  Motion here is in cursor positions, not in characters, so a single call to
  the @fun{pango:layout-move-cursor-visually} function may move the cursor over
  multiple characters when multiple characters combine to form a single
  grapheme.
  @see-class{pango:layout}"
  (cffi:with-foreign-objects ((new-index :int) (new-trailing :int))
    (%layout-move-cursor-visually layout
                                  strong
                                  index
                                  trailing
                                  direction
                                  new-index
                                  new-trailing)
    (values (cffi:mem-ref new-index :int)
            (cffi:mem-ref new-trailing :int))))

(export 'layout-move-cursor-visually)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_extents" %layout-extents) :void
  (layout (g:object layout))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun layout-extents (layout ink logical)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[layout]{a @class{pango:layout} object}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the extents
    of the layout as drawn, @code{nil}}
  @argument[logical]{a @symbol{pango:rectangle} instance to fill with the
    logical extents of the layout, or @code{nil}}
  @begin{short}
    Computes the logical and ink extents of the Pango layout.
  @end{short}
  Logical extents are usually what you want for positioning things. Note that
  both extents may have non-zero @arg{x} and @arg{y}. You may want to use those
  to offset where you render the layout. Not doing that is a very typical bug
  that shows up as right-to-left layouts not being correctly positioned in a
  layout with a set width.

  The extents are given in layout coordinates and in Pango units. Layout
  coordinates begin at the top left corner of the layout.
  @see-class{pango:layout}
  @see-symbol{pango:rectangle}"
  (%layout-extents layout
                   (or ink (cffi:null-pointer))
                   (or logical (cffi:null-pointer))))

(export 'layout-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_pixel_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_pixel_extents" %layout-pixel-extents) :void
  (layout (g:object layout))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun layout-pixel-extents (layout ink logical)
 #+liber-documentation
 "@version{2024-3-6}
  @argument[layout]{a @class{pango:layout} object}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the extents
    of the layout as drawn}
  @argument[logical]{a @symbol{pango:rectangle} instance to fill with the
    logical extents of the layout}
  @begin{short}
    Computes the logical and ink extents of the Pango layout in device units.
  @end{short}
  This function just calls the @fun{pango:layout-extents} function followed by
  two @fun{pango:extents-to-pixels} function calls, rounding the @arg{ink} and
  @arg{logical} values such that the rounded rectangles fully contain the
  unrounded one, that is, passes them as first argument to the
  @fun{pango:extents-to-pixels} function.
  @see-class{pango:layout}
  @see-function{pango:layout-extents}
  @see-function{pango:extents-to-pixels}"
  (%layout-pixel-extents layout
                         (or ink (cffi:null-pointer))
                         (or logical (cffi:null-pointer))))

(export 'layout-pixel-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_size" %layout-size) :void
  (layout (g:object layout))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun layout-size (layout)
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    @code{width}  -- an integer with the logical width @br{}
    @code{height} -- an integer with the logical height
  @end{return}
  @begin{short}
    Determines the logical width and height of a layout in Pango units, device
    units scaled by the @var{pango:+scale+} constant.
  @end{short}
  This is simply a convenience function around the @fun{pango:layout-extents}
  function.
  @see-class{pango:layout}
  @see-function{pango:layout-extents}
  @see-variable{pango:+scale+}"
  (cffi:with-foreign-objects ((width :int) (height :int))
    (%layout-size layout width height)
    (values (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'layout-size)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_pixel_size ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_pixel_size" %layout-get-pixel-size) :void
  (layout (g:object layout))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun layout-pixel-size (layout)
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    @code{width} - an integer with the logical width @br{}
    @code{height} - an integer with the logical height
  @end{return}
  @begin{short}
    Determines the logical width and height of a Pango layout in device units.
  @end{short}
  The @fun{pango:layout-size} function returns the width and height scaled by
  @var{pango:+scale+}. This is simply a convenience function around the
  @fun{pango:layout-pixel-extents} function.
  @see-class{pango:layout}
  @see-function{pango:layout-size}
  @see-function{pango:layout-pixel-extents}"
  (cffi:with-foreign-objects ((width :int) (height :int))
    (%layout-get-pixel-size layout width height)
    (values (cffi:mem-ref width :int)
            (cffi:mem-ref height :int))))

(export 'layout-pixel-size)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_baseline ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_baseline" layout-baseline) :int
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @return{The integer with the baseline of first line, from top of
    @arg{layout}.}
  @begin{short}
    Gets the y position of baseline of the first line in the layout.
  @end{short}
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-baseline)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line_count ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_line_count" layout-line-count) :int
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @return{The integer with the line count.}
  @begin{short}
    Retrieves the count of lines for the layout.
  @end{short}
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-line-count)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_line" layout-line) (g:boxed layout-line)
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @argument[linenum]{an integer with the index of a line, which must be between
    0 and @code{(pango:layout-line-count layout)} - 1, inclusive}
  @begin{return}
    The requested @class{pango:layout-line} instance, or @code{nil} if the
    index is out of range. This layout line can be ref'ed and retained, but
    will become invalid if changes are made to the Pango layout.
  @end{return}
  @begin{short}
    Retrieves a particular line from a Pango layout.
  @end{short}
  Use the faster @fun{pango:layout-line-readonly} function if you do not plan
  to modify the contents of the line (glyphs, glyph widths, etc.).
  @see-class{pango:layout}
  @see-class{pango:layout-line}
  @see-function{pango:layout-line-readonly}"
  (layout (g:object layout))
  (line :int))

(export 'layout-line)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_line_readonly ()
;;; ----------------------------------------------------------------------------

;;; TODO: Do we need a second function. Which is the better implementation?

(cffi:defcfun ("pango_layout_get_line_readonly" layout-line-readonly)
    (g:boxed layout-line)
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @argument[line]{an integer with the index of a line, which must be between 0
    and (@code{pango:layout-line-count layout)} - 1, inclusive}
  @begin{return}
    The requested @class{pango:layout-line} instance, or @code{nil} if the index
    is out of range. This layout line can be ref'ed and retained, but will
    become invalid if changes are made to the PangoLayout. No changes should be
    made to the line.
  @end{return}
  @begin{short}
    Retrieves a particular line from a Pango layout.
  @end{short}
  This is a faster alternative to the @fun{pango:layout-line} function, but the
  user is not expected to modify the contents of the line (glyphs, glyph widths,
  etc.).
  @see-class{pango:layout}
  @see-function{pango:layout-line}"
  (layout (g:object layout))
  (line :int))

(export 'layout-line-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_lines ()
;;; ----------------------------------------------------------------------------

;; TODO: Check again the implementation of g:slist-t. The g:slist-t type does
;; not work for a boxed type which is not allowed to be freed. Improve the
;; implementation of g:slist-t. A workaraound is to use the
;; pango:layout-line function in a loop.

(defun layout-lines (layout)
 #+liber-documentation
 "@version{2024-2-25}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    A list containing the @class{pango:layout-line} instances in the layout.
    This points to internal data of the Pango layout and must be used with
    care. It will become invalid on any change to the text of the layout or
    properties.
  @end{return}
  @begin{short}
    Returns the lines of the layout as a list.
  @end{short}
  Use the faster @fun{pango:layout-lines-readonly} function if you do not plan
  to modify the contents of the lines (glyphs, glyph widths, etc.).
  @see-class{pango:layout}
  @see-class{pango:layout-line}"
  (iter (for index from 0 below (layout-line-count layout))
        (collect (layout-line layout index))))

(export 'layout-lines)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_lines_readonly ()
;;; ----------------------------------------------------------------------------

;; TODO: Check again the implementation of g:slist-t. See the comment above.
;; Do we need a second function. Which is the better implementation?

(defun layout-lines-readonly (layout)
 #+liber-documentation
 "@version{2023-2-7}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    A list containing the @class{pango:layout-line} instances in the layout.
    This points to internal data of the Pango layout and must be used with care.
    It will become invalid on any change to the layout's text or properties. No
    changes should be made to the lines.
  @end{return}
  @begin{short}
    Returns the lines of the layout as a list.
  @end{short}
  This is a faster alternative to the @fun{pango:layout-lines} function, but
  the user is not expected to modify the contents of the lines (glyphs, glyph
  widths, etc.).
  @see-class{pango:layout}
  @see-class{pango:layout-line}
  @see-function{pango:layout-lines}"
  (iter (for count from 0 below (layout-line-count layout))
        (collect (layout-line-readonly layout count))))

(export 'layout-lines-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_get_iter ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_get_iter" layout-iter)
    (g:boxed layout-iter :return)
 #+liber-documentation
 "@version{2023-2-11}
  @argument[layout]{a @class{pango:layout} object}
  @begin{return}
    The new @symbol{pango:layout-iter} instance.
  @end{return}
  @begin{short}
    Returns an iterator to iterate over the visual extents of the layout.
  @end{short}
  @see-class{pango:layout}"
  (layout (g:object layout)))

(export 'layout-iter)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_copy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_copy" layout-iter-copy)
    (g:boxed layout-iter :return)
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The newly allocated @class{pango:layout-iter} instance.}
  @begin{short}
    Copies a layout iterator.
  @end{short}
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-copy)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_free ()                              not needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_free" layout-iter-free) :void
 #+liber-documentation
 "@version{#2021-1-15}
  @argument[iter]{a @class{pango:layout-iter} instance, may be NULL}
  @begin{short}
    Frees an iterator that is no longer in use.
  @end{short}
  @see-class{pango:layout-iter}"
  (iter (g:object layout-iter)))

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_run ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_next_run" layout-iter-next-run) :boolean
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The boolean whether motion was possible.}
  @begin{short}
    Moves @arg{iter} forward to the next run in visual order.
  @end{short}
  If @arg{iter} was already at the end of the layout, returns @em{false}.
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-next-run)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_char ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_next_char" layout-iter-next-char) :boolean
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The boolean whether motion was possible.}
  @begin{short}
    Moves @arg{iter} forward to the next character in visual order.
  @end{short}
  If @arg{iter} was already at the end of the layout, returns @em{false}.
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-next-char)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_cluster ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_next_cluster" layout-iter-next-cluster)
    :boolean
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The boolean whether motion was possible.}
  @begin{short}
    Moves @arg{iter} forward to the next cluster in visual order.
  @end{short}
  If @arg{iter} was already at the end of the layout, returns @em{false}.
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-next-cluster)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_next_line ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_next_line" layout-iter-next-line) :boolean
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The boolean whether motion was possible.}
  @begin{short}
    Moves @arg{iter} forward to the start of the next line.
  @end{short}
  If @arg{iter} is already on the last line, returns @em{false}.
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-next-line)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_at_last_line ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_at_last_line" layout-iter-at-last-line)
    :boolean
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{@em{True} if @arg{iter} is on the last line.}
  @begin{short}
    Determines whether @arg{iter} is on the last line of the layout.
  @end{short}
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-at-last-line)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_index ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_index" layout-iter-index) :int
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The integer with the current byte index.}
  @begin{short}
    Gets the current byte index.
  @end{short}
  Note that iterating forward by char moves in visual order, not logical order,
  so indexes may not be sequential. Also, the index may be equal to the length
  of the text in the layout, if on the NULL run (see the
  @fun{pango:layout-iter-run} function).
  @see-class{pango:layout-iter}
  @see-function{pango:layout-iter-run}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-index)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_baseline ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_baseline" layout-iter-baseline) :int
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The integer with the baseline of current line.}
  @begin{short}
    Gets the y position of the current line's baseline, in layout coordinates
    (origin at top left of the entire layout).
  @end{short}
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-baseline)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_run" layout-iter-run)
    (g:boxed glyph-item :return)
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The @class{pango:glyph-item} instance with the current run.}
  @begin{short}
    Gets the current run.
  @end{short}
  When iterating by run, at the end of each line, there is a position with a
  NULL run, so this function can return @code{nil}. The NULL run at the end of
  each line ensures that all lines have at least one run, even lines consisting
  of only a newline.

  Use the faster @fun{pango:layout-iter-run-readonly} function if you do not
  plan to modify the contents of the run (glyphs, glyph widths, etc.).
  @see-class{pango:layout-iter}
  @see-class{pango:glyph-item}
  @see-function{pango:layout-iter-run-readonly}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-run)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run_readonly ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_run_readonly" layout-iter-run-readonly)
    (g:boxed glyph-item)
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The @class{pango:layout-iter} instance with the current run, that
    should not be modified.}
  @begin{short}
    Gets the current run.
  @end{short}
  When iterating by run, at the end of each line, there is a position with a
  NULL run, so this function can return @code{nil}. The NULL run at the end of
  each line ensures that all lines have at least one run, even lines consisting
  of only a newline.

  This is a faster alternative to the @fun{pango:layout-iter-run} function, but
  the user is not expected to modify the contents of the run (glyphs,
  glyph widths, etc.).
  @see-class{pango:layout-iter}
  @see-function{pango:layout-iter-run}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-run-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_line" layout-iter-line)
    (g:boxed layout-line)
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The @class{pango:layout-line} instance with the current line.}
  @begin{short}
    Gets the current line.
  @end{short}
  Use the faster @fun{pango:layout-iter-line-readonly} function if you do not
  plan to modify the contents of the line (glyphs, glyph widths, etc.).
  @see-class{pango:layout-iter}
  @see-class{pango:layout-line}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-line)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_readonly ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_line_readonly" layout-iter-line-readonly)
    (g:boxed layout-line)
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The @class{pango:layout-line} instance with the current line, that
    should not be modified.}
  @begin{short}
    Gets the current line for read-only access.
  @end{short}
  This is a faster alternative to the @fun{pango:layout-iter-line} function,
  but the user is not expected to modify the contents of the line (glyphs,
  glyph widths, etc.).
  @see-class{pango:layout-iter}
  @see-class{pango:layout-line}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-line-readonly)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_layout ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_layout" layout-iter-layout)
    (g:object layout)
 #+liber-documentation
 "@version{2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @return{The @class{pango:layout} object with the layout associated with
    @arg{iter}.}
  @begin{short}
    Gets the layout associated with a Pango layout iterator.
  @end{short}
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter)))

(export 'layout-iter-layout)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_char_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_char_extents" layout-iter-char-extents)
    :void
 #+liber-documentation
 "@version{2024-3-7}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[extents]{a @symbol{pango:rectangle} instance to fill with logical
    extents}
  @begin{short}
    Gets the extents of the current character, in layout coordinates.
  @end{short}
  Origin is the top left of the entire layout. Only logical extents can sensibly
  be obtained for characters. Ink extents make sense only down to the level of
  clusters.
  @see-class{pango:layout-iter}"
  (iter (g:boxed layout-iter))
  (extents (:pointer (:struct rectangle))))

(export 'layout-iter-char-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_cluster_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_cluster_extents"
                %layout-iter-cluster-extents) :void
  (iter (g:boxed layout-iter))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun layout-iter-cluster-extents (iter ink logical)
 #+liber-documentation
 "@version{2024-3-7}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the values
    of the ink extents,  or @code{nil}}
  @argument[logical]{a @symbol{pango:rectangle} instance to fill with the values
    of the logical extents, or @code{nil}}
  @begin{short}
    Gets the extents of the current cluster, in layout coordinates, origin is
    the top left of the entire layout.
  @end{short}
  @see-class{pango:layout-iter}"
  (%layout-iter-cluster-extents iter
                                (or ink (cffi:null-pointer))
                                (or logical (cffi:null-pointer))))

(export 'layout-iter-cluster-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_run_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_run_extents" %layout-iter-run-extents)
    :void
  (iter (g:boxed layout-iter))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun layout-iter-run-extents (iter ink logical)
 #+liber-documentation
 "@version{2024-3-7}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the values
    of the ink extents, or @code{nil}}
  @argument[logical]{a @symbol{pango:rectangle} instance to fill with the values
    of the logical extents, or @code{nil}}
  @begin{short}
    Gets the extents of the current run in layout coordinates (origin is the
    top left of the entire layout).
  @end{short}
  @see-class{pango:layout-iter}
  @see-symbol{pango:rectangle}"
  (%layout-iter-run-extents iter
                            (or ink (cffi:null-pointer))
                            (or logical (cffi:null-pointer))))

(export 'layout-iter-run-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_yrange ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_line_yrange" %layout-iter-line-yrange)
    :void
  (iter (g:boxed layout-iter))
  (y0 (:pointer :int))
  (y1 (:pointer :int)))

(defun layout-iter-line-yrange (iter)
 #+liber-documentation
 "@version{2023-4-2}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @begin{return}
    @arg{y0} -- an integer with the start of line @br{}
    @arg{y1} -- an integer with the end of line
  @end{return}
  @begin{short}
    Divides the vertical space in the Pango layout being iterated over between
    the lines in the layout, and returns the space belonging to the current
    line.
  @end{short}
  A line's range includes the line's logical extents, plus half of the spacing
  above and below the line, if the @fun{pango:layout-spacing} function has been
  called to set layout spacing. The @arg{y0} and @arg{y1} positions are in
  layout coordinates (origin at top left of the entire layout).
  @begin[Note]{dictionary}
    Since 1.44, Pango uses line heights for placing lines, and there may be
    gaps between the ranges returned by this function.
  @end{dictionary}
  @see-class{pango:layout-iter}
  @see-function{pango:layout-spacing}"
  (cffi:with-foreign-objects ((y0 :int) (y1 :int))
    (%layout-iter-line-yrange iter y0 y1)
    (values (cffi:mem-ref y0 :int)
            (cffi:mem-ref y1 :int))))

(export 'layout-iter-line-yrange)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_line_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_line_extents" %layout-iter-line-extents)
    :void
  (iter (g:boxed layout-iter))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun layout-iter-line-extents (iter ink logical)
 #+liber-documentation
 "@version{2024-3-7}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the values
    of the ink extents, or @code{nil}}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the values
    of the logical extents, or @code{nil}}
  @begin{short}
    Obtains the extents of the current line.
  @end{short}
  Extents are in layout coordinates (origin is the top-left corner of the
  entire Pango layout). Thus the extents returned by this function will be the
  same width/height but not at the same x/y as the extents returned from the
  @fun{pango:layout-line-extents} function.
  @see-class{pango:layout-iter}
  @see-function{pango:layout-line-extents}"
  (%layout-iter-line-extents iter
                             (or ink (cffi:null-pointer))
                             (or logical (cffi:null-pointer))))

(export 'layout-iter-line-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_iter_get_layout_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_iter_get_layout_extents"
                %layout-iter-layout-extents) :void
  (iter (g:boxed layout-iter))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun layout-iter-layout-extents (iter ink logical)
 #+liber-documentation
 "@version{2024-3-7}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the values
    of the ink extents, or @code{nil}}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the values
    of the logical extents, or @code{nil}}
  @begin{short}
    Obtains the extents of the Pango layout being iterated over.
  @end{short}
  @see-class{pango:layout-iter}
  @see-symbol{pango:rectangle}"
  (%layout-iter-layout-extents iter
                               (or ink (cffi:null-pointer))
                               (or logical (cffi:null-pointer))))

(export 'layout-iter-layout-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_ref ()                               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_line_ref" %layout-line-ref) :pointer
 #+liber-documentation
 "@version{#2021-1-16}
  @argument[line]{a @class{pango:layout-line} instance, may be NULL}
  @return{The @class{pango:layout-line} instance with the line passed in.}
  @begin{short}
    Increase the reference count of a @class{pango:layout-line} instance by one.
  @end{short}
  @see-class{pango:layout-line}"
  (line :pointer))

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_unref ()                             not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_line_unref" layout-line-unref) :void
 #+liber-documentation
 "@version{#2021-1-16}
  @argument[line]{a @class{pango:layout-line} instance}
  @begin{short}
    Decrease the reference count of a @class{pango:layout-line} instance by one.
  @end{short}
  If the result is zero, the line and all associated memory will be freed.
  @see-class{pango:layout-line}"
  (line (g:boxed layout-line)))

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_line_get_extents" %layout-line-extents) :void
  (line (g:boxed layout-line))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun layout-line-extents (line ink logical)
 #+liber-documentation
 "@version{2024-3-7}
  @argument[line]{a @class{pango:layout-line} instance}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the values
    of the ink extents of the glyph string as drawn,  or @code{nil}}
  @argument[logical]{a @symbol{pango:rectangle} instance to fill with the values
    of the logical extents of the glyph string as drawn, or @code{nil}}
  @begin{short}
    Computes the logical and ink extents of a layout line.
  @end{short}
  See the @fun{pango:font-glyph-extents} function for details about the
  interpretation of the rectangles.
  @see-class{pango:layout-line}
  @see-function{pango:font-glyph-extents}"
  (%layout-line-extents line
                        (or ink (cffi:null-pointer))
                        (or logical (cffi:null-pointer))))

(export 'layout-line-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_height ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(cffi:defcfun ("pango_layout_line_get_height" %layout-line-height) :void
  (line (g:boxed layout-line))
  (height (:pointer :int)))

#+pango-1-44
(defun layout-line-height (line)
 #+liber-documentation
 "@version{#2023-2-11}
  @argument[line]{a @class{pango:layout-line} instance}
  @return{The integer with the line height.}
  @begin{short}
    Computes the height of the line, i.e. the distance between this and the
    previous lines baseline.
  @end{short}

  Since 1.44
  @see-class{pango:layout-line}"
  (cffi:with-foreign-object (height :int)
    (%layout-line-height line height)
    (unless (cffi:null-pointer-p height)
      (values (cffi:mem-ref height :int)))))

#+pango-1-44
(export 'layout-line-height)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_length ()
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_layout_line_get_length" layout-line-length) :int
 #+liber-documentation
 "@version{#2023-7-17}
  @argument[line]{a @class{pango:layout-line} instance}
  @return{The integer with the length of the line.}
  @begin{short}
    Returns the length of the line, in bytes.
  @end{short}

  Since 1.50
  @see-class{pango:layout-line}"
  (line (g:boxed layout-line)))

#+pango-1-50
(export 'layout-line-length)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_pixel_extents ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_line_get_pixel_extents" %layout-line-pixel-extents)
    :void
  (line (g:boxed layout-line))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun layout-line-pixel-extents (line ink logical)
 #+liber-documentation
 "@version{2024-3-7}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[ink]{a @symbol{pango:rectangle} instance to fill with the values
    of the ink extents of the glyph string as drawn, or @code{nil}}
  @argument[logical]{a @symbol{pango:rectangle} instance to fill with the values
    of the logical extents of the glyph string as drawn, or @code{nil}}
  @begin{short}
    Computes the logical and ink extents of @arg{line} in device units.
  @end{short}
  This function just calls the @fun{pango:layout-line-extents} function
  followed by two @fun{pango:extents-to-pixels} function calls, rounding
  @arg{ink} and @arg{logical} such that the rounded rectangles fully contain
  the unrounded one, that is, passes them as first argument to the
  @fun{pango:extents-to-pixels} function.
  @see-class{pango:layout-line}
  @see-function{pango:layout-line-extents}
  @see-function{pango:extents-to-pixels}"
  (%layout-line-pixel-extents line
                              (or ink (cffi:null-pointer))
                              (or logical (cffi:null-pointer))))

(export 'layout-line-pixel-extents)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_resolved_direction ()
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_layout_line_get_resolved_direction"
                layout-line-resolved-direction) direction
 #+liber-documentation
 "@version{#2023-7-17}
  @argument[line]{a @class{pango:layout-line} instance}
  @return{The @symbol{pango:direction} value with the resolved direction of the
    line.}
  @begin{short}
    Returns the resolved direction of the line.
  @end{short}

  Since 1.50
  @see-class{pango:layout-line}"
  (line (g:boxed layout-line)))

#+pango-1-50
(export 'layout-line-resolved-direction)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_start_index ()
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_layout_line_start_index" layout-line-start-index) :int
 #+liber-documentation
 "@version{#2023-7-17}
  @argument[line]{a @class{pango:layout-line} instance}
  @return{The integer with the start index of the line.}
  @begin{short}
    Returns the start index of the line, as byte index into the text of the
    layout.
  @end{short}

  Since 1.50
  @see-class{pango:layout-line}"
  (line (g:boxed layout-line)))

#+pango-1-50
(export 'layout-line-start-index)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_get_x_ranges ()
;;; ----------------------------------------------------------------------------

;; TODO: Return a Lisp list with the values.

(cffi:defcfun ("pango_layout_line_get_x_ranges" layout-line-x-ranges) :void
 #+liber-documentation
 "@version{#2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[start-index]{an integer with the start byte index of the logical
    range. If this value is less than the start index for the line, then the
    first range will extend all the way to the leading edge of the layout.
    Otherwise it will start at the leading edge of the first character.}
  @argument[end-index]{an integer with the ending byte index of the logical
    range. If this value is greater than the end index for the line, then the
    last range will extend all the way to the trailing edge of the layout.
    Otherwise, it will end at the trailing edge of the last character.}
  @begin{return}
    @code{ranges} -- a pointer to an array of ranges. The array will be of
    length 2*n_ranges, with each range starting at (*ranges)[2*n] and of
    width (*ranges)[2*n + 1] - (*ranges)[2*n]. This array must be freed with
    @fun{glib:free}. The coordinates are relative to the layout and are in
    Pango units @br{}
    @code{n-ranges} -- The number of ranges stored in ranges.
  @end{return}
  @begin{short}
    Gets a list of visual ranges corresponding to a given logical range.
  @end{short}
  This list is not necessarily minimal - there may be consecutive ranges which
  are adjacent. The ranges will be sorted from left to right. The ranges are
  with respect to the left edge of the entire layout, not with respect to the
  line.
  @see-class{pango:layout-line}"
  (line (g:boxed layout-line))
  (start-index :int)
  (end-index :int)
  (ranges :pointer)
  (n-ranges (:pointer :int)))

(export 'layout-line-x-ranges)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_index_to_x ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_line_index_to_x" %layout-line-index-to-x) :void
  (line (g:boxed layout-line))
  (index :int)
  (trailing :boolean)
  (xpos (:pointer :int)))

(defun layout-line-index-to-x (line index trailing)
 #+liber-documentation
 "@version{#2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[index]{an integer with the byte offset of a grapheme within the
    layout}
  @argument[trailing]{a boolean indicating the edge of the grapheme to retrieve
    the position of, if @em{true}, the trailing edge of the grapheme, if
    @em{false}, the leading of the grapheme}
  @begin{return}
    An integer with the x offset (in Pango unit).
  @end{return}
  @begin{short}
    Converts an index within a line to a x position.
  @end{short}
  @see-class{pango:layout-line}"
  (cffi:with-foreign-object (xpos :int)
    (%layout-line-index-to-x line index trailing xpos)
    (values (cffi:mem-ref xpos :int))))

(export 'layout-line-index-to-x)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_is_paragraph_start ()
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_layout_line_is_paragraph_start"
                layout-line-is-paragraph-start) :boolean
 #+liber-documentation
 "@version{#2023-7-17}
  @argument[line]{a @class{pango:layout-line} instance}
  @return{@em{True} if this is the first line.}
  @begin{short}
    Returns whether this is the first line of the paragraph.
  @end{short}

  Since 1.50
  @see-class{pango:layout-line}"
  (line (g:boxed layout-line)))

#+pango-1-50
(export 'layout-line-is-paragraph-start)

;;; ----------------------------------------------------------------------------
;;; pango_layout_line_x_to_index ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_layout_line_x_to_index" %layout-line-x-to-index) :boolean
  (line  (g:boxed layout-line))
  (xpos :int)
  (index (:pointer :int))
  (trailing (:pointer :int)))

(defun layout-line-x-to-index (line xpos)
 #+liber-documentation
 "@version{#2023-2-11}
  @argument[iter]{a @class{pango:layout-iter} instance}
  @argument[xpos]{an integer with the x offset (in Pango units) from the left
    edge of the line}
  @begin{return}
    @code{index} -- an integer with the calculated byte index for the grapheme
    in which the user clicked @br{}
    @code{trailing} -- an integer indicating where in the grapheme the user
    clicked. It will either be zero, or the number of characters in the
    grapheme. 0 represents the leading edge of the grapheme.
    @code{bool} -- @em{false} if @arg{xpos} was outside the line, @em{true}
    if inside
  @end{return}
  @begin{short}
    Converts from x offset to the byte index of the corresponding character
    within the text of the layout.
  @end{short}
  If @arg{xpos} is outside the line, @arg{index} and @arg{trailing} will point
  to the very first or very last position in the line. This determination is
  based on the resolved direction of the paragraph; for example, if the resolved
  direction is right-to-left, then an x position to the right of the line (after
  it) results in 0 being stored in @arg{index} and @arg{trailing}. An x position
  to the left of the line results in @arg{index} pointing to the (logical) last
  grapheme in the line and trailing being set to the number of characters in
  that grapheme. The reverse is true for a left-to-right line.
  @see-class{pango:layout-line-x-to-index}"
  (cffi:with-foreign-objects ((index :int) (trailing :int))
    (let ((bool (%layout-line-x-to-index line xpos index trailing)))
      (values (cffi:mem-ref index :int)
              (cffi:mem-ref trailing :int)
              bool))))

(export 'layout-line-x-to-index)

;;; --- End of file pango.layout.lisp ------------------------------------------
