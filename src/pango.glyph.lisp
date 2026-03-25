;;; ----------------------------------------------------------------------------
;;; pango.glyph.lisp
;;;
;;; The documentation in this file is taken from the Pango Reference Manual
;;; version 1.56 and modified to document the Lisp binding to the Pango
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2026 Dieter Kaiser
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
;;; Glyph Storage
;;;
;;;     Structures for storing information about glyphs
;;;
;;; Types and Values
;;;
;;;     PangoGlyph
;;;
;;;     PANGO_GLYPH_EMPTY
;;;     PANGO_GLYPH_INVALID_INPUT
;;;     PANGO_GLYPH_UNKNOWN_FLAG
;;;
;;;     PangoGlyphInfo
;;;     PangoGlyphGeometry
;;;     PangoGlyphUnit
;;;     PangoGlyphVisAttr
;;;
;;;     PangoGlyphString
;;;     PangoGlyphItem
;;;     PangoGlyphItemIter
;;;
;;; Functions
;;;
;;;     PANGO_GET_UNKNOWN_GLYPH
;;;
;;;     pango_glyph_string_new
;;;     pango_glyph_string_copy
;;;     pango_glyph_string_set_size
;;;     pango_glyph_string_free                             not needed
;;;     pango_glyph_string_extents
;;;     pango_glyph_string_extents_range
;;;     pango_glyph_string_get_width
;;;     pango_glyph_string_index_to_x
;;;     pango_glyph_string_index_to_x_full                  Since 1.50
;;;     pango_glyph_string_x_to_index
;;;     pango_glyph_string_get_logical_widths
;;;
;;;     pango_glyph_item_copy
;;;     pango_glyph_item_free                               not needed
;;;     pango_glyph_item_split
;;;     pango_glyph_item_apply_attrs
;;;     pango_glyph_item_letter_space
;;;     pango_glyph_item_get_logical_widths
;;;
;;;     pango_glyph_item_iter_copy
;;;     pango_glyph_item_iter_free
;;;     pango_glyph_item_iter_init_start
;;;     pango_glyph_item_iter_init_end
;;;     pango_glyph_item_iter_next_cluster
;;;     pango_glyph_item_iter_prev_cluster
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ├── PangoGlyphItem
;;;     ├── PangoGlyphItemIter
;;;     ╰── PangoGlyphString
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PANGO_GLYPH_EMPTY
;;;
;;; #define PANGO_GLYPH_EMPTY ((PangoGlyph)0x0FFFFFFF)
;;;
;;; The PANGO_GLYPH_EMPTY macro represents a PangoGlyph value that has a special
;;; meaning, which is a zero-width empty glyph. This is useful for example in
;;; shaper modules, to use as the glyph for various zero-width Unicode
;;; characters (those passing pango_is_zero_width()).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GLYPH_INVALID_INPUT
;;;
;;; #define PANGO_GLYPH_INVALID_INPUT ((PangoGlyph)0xFFFFFFFF)
;;;
;;; The PANGO_GLYPH_EMPTY macro represents a PangoGlyph value that has a special
;;; meaning of invalid input. PangoLayout produces one such glyph per invalid
;;; input UTF-8 byte and such a glyph is rendered as a crossed box. Note that
;;; this value is defined such that it has the PANGO_GLYPH_UNKNOWN_FLAG on.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GLYPH_UNKNOWN_FLAG
;;;
;;; #define PANGO_GLYPH_UNKNOWN_FLAG ((PangoGlyph)0x10000000)
;;;
;;; The PANGO_GLYPH_UNKNOWN_FLAG macro is a flag value that can be added to a
;;; gunichar value of a valid Unicode character, to produce a PangoGlyph value,
;;; representing an unknown-character glyph for the respective gunichar.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoGlyph
;;; ----------------------------------------------------------------------------

(cffi:defctype glyph :uint32)

#+liber-documentation
(setf (liber:alias-for-symbol 'glyph)
      "Type"
      (liber:symbol-documentation 'glyph)
 "@version{2025-12-09}
  @begin{short}
    The @sym{pango:glyph} type represents a single glyph in the output form of
    a string.
  @end{short}
  This type is implemented as the @code{:uint32} foreign type.
  @see-class{pango:glyph-item}
  @see-class{pango:glyph-string}")

(export 'glyph)

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphInfo
;;;
;;; struct PangoGlyphInfo {
;;;   PangoGlyph    glyph;
;;;   PangoGlyphGeometry geometry;
;;;   PangoGlyphVisAttr  attr;
;;; };
;;;
;;; The PangoGlyphInfo structure represents a single glyph together with
;;; positioning information and visual attributes. It contains the following
;;; fields.
;;;
;;; PangoGlyph glyph;
;;;     the glyph itself.
;;;
;;; PangoGlyphGeometry geometry;
;;;     the positional information about the glyph.
;;;
;;; PangoGlyphVisAttr attr;
;;;     the visual attributes of the glyph.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphGeometry
;;;
;;; struct PangoGlyphGeometry {
;;;   PangoGlyphUnit width;
;;;   PangoGlyphUnit x_offset;
;;;   PangoGlyphUnit y_offset;
;;; };
;;;
;;; The PangoGlyphGeometry structure contains width and positioning information
;;; for a single glyph.
;;;
;;; PangoGlyphUnit width;
;;;     the logical width to use for the the character.
;;;
;;; PangoGlyphUnit x_offset;
;;;     horizontal offset from nominal character position.
;;;
;;; PangoGlyphUnit y_offset;
;;;     vertical offset from nominal character position.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoGlyphUnit
;;;
;;; typedef gint32 PangoGlyphUnit;
;;;
;;; The PangoGlyphUnit type is used to store dimensions within Pango. Dimensions
;;; are stored in 1/PANGO_SCALE of a device unit. (A device unit might be a
;;; pixel for screen display, or a point on a printer.) PANGO_SCALE is currently
;;; 1024, and may change in the future (unlikely though), but you should not
;;; depend on its exact value. The PANGO_PIXELS() macro can be used to convert
;;; from glyph units into device units with correct rounding.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphVisAttr
;;;
;;; struct PangoGlyphVisAttr {
;;;   guint is_cluster_start : 1;
;;; };
;;;
;;; The PangoGlyphVisAttr is used to communicate information between the shaping
;;; phase and the rendering phase. More attributes may be added in the future.
;;;
;;; guint is_cluster_start : 1;
;;;     set for the first logical glyph in each cluster. (Clusters are stored in
;;;     visual order, within the cluster, glyphs are always ordered in logical
;;;     order, since visual order is meaningless; that is, in Arabic text,
;;;     accent glyphs follow the glyphs for the base character.)
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoGlyphString
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque glyph-string "PangoGlyphString"
  :export t
  :type-initializer "pango_glyph_string_get_type"
  :alloc (%glyph-string-new))

#+liber-documentation
(setf (liber:alias-for-class 'glyph-string)
      "GBoxed"
      (documentation 'glyph-string 'type)
 "@version{2026-03-08}
  @begin{declaration}
(glib:define-gboxed-opaque glyph-string \"PangoGlyphString\"
  :export t
  :type-initializer \"pango_glyph_string_get_type\"
  :alloc (%glyph-string-new))
  @end{declaration}
  @begin{short}
    The @class{pango:glyph-string} structure is used to store strings of glyphs
    with geometry and visual attribute information.
  @end{short}
  The storage for the glyph information is owned by the structure which
  simplifies memory management.
  @begin[Examples]{dictionary}
    Create a glyph string using the @fun{pango:shape} function.
    @begin{pre}
(let* ((text \"Zwölf Ägypter gehen über die Straße.\")
       (fontmap (pango:cairo-font-map-default))
       (context (pango:font-map-create-context fontmap))
       (items (pango:itemize context
                             text
                             0
                             (babel:string-size-in-octets text)
                             nil
                             nil))
       (item (first items))
       (glyphs (pango:shape text
                            (pango:item-length item)
                            (pango:item-analysis item))))
  glyphs)
=> #<PANGO:GLYPH-STRING {10055AAA13@}>
    @end{pre}
  @end{dictionary}
  @see-constructor{pango:glyph-string-new}
  @see-constructor{pango:glyph-string-copy}
  @see-class{pango:glyph-item}
  @see-function{pango:shape}")

;;; ----------------------------------------------------------------------------
;;; PangoGlyphItem
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque glyph-item "PangoGlyphItem"
  :export t
  :type-initializer "pango_glyph_item_get_type"
  :alloc (error "PangoGlyphItem cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'glyph-item)
      "GBoxed"
      (documentation 'glyph-item 'type)
 "@version{2026-03-08}
  @begin{declaration}
(glib:define-gboxed-opaque glyph-item \"PangoGlyphItem\"
  :export t
  :type-initializer \"pango_glyph_item_get_type\"
  :alloc (error \"PangoGlyphItem cannot be created from the Lisp side.\"))
  @end{declaration}
  @begin{short}
    The @class{pango:glyph-item} structure is a pair of a @class{pango:item}
    instance and the glyphs resulting from shaping the text corresponding to
    an item.
  @end{short}
  As an example of the usage of the @class{pango:glyph-item} structure, the
  results of shaping text with the @class{pango:layout} class is a list of
  @class{pango:layout-line} objects, each of which contains a list of
  @class{pango:glyph-item} instances.
  @see-constructor{pango:glyph-item-copy}
  @see-class{pango:item}
  @see-class{pango:layout}
  @see-class{pango:layout-line}")

(export 'glyph-item)

;;; ----------------------------------------------------------------------------
;;; struct PangoGlyphItemIter
;;;
;;; struct PangoGlyphItemIter {
;;;   PangoGlyphItem *glyph_item;
;;;   const gchar *text;
;;;
;;;   int start_glyph;
;;;   int start_index;
;;;   int start_char;
;;;
;;;   int end_glyph;
;;;   int end_index;
;;;   int end_char;
;;; };
;;;
;;; A PangoGlyphItemIter is an iterator over the clusters in a PangoGlyphItem.
;;; The forward direction of the iterator is the logical direction of text. That
;;; is, with increasing start_index and start_char values. If glyph_item is
;;; right-to-left (that is, if glyph_item->item->analysis.level is odd), then
;;; start_glyph decreases as the iterator moves forward. Moreover, in
;;; right-to-left cases, start_glyph is greater than end_glyph. An iterator
;;; should be initialized using either of pango_glyph_item_iter_init_start() and
;;; pango_glyph_item_iter_init_end(), for forward and backward iteration
;;; respectively, and walked over using any desired mixture of
;;; pango_glyph_item_iter_next_cluster() and
;;; pango_glyph_item_iter_prev_cluster(). A common idiom for doing a forward
;;; iteration over the clusters is:
;;;
;;; PangoGlyphItemIter cluster_iter;
;;;
;;; gboolean have_cluster;
;;; for (have_cluster = pango_glyph_item_iter_init_start (&cluster_iter,
;;;                                                       glyph_item, text);
;;;      have_cluster;
;;;      have_cluster = pango_glyph_item_iter_next_cluster (&cluster_iter))
;;; {
;;;   ...
;;; }
;;;
;;; Note that text is the start of the text for layout, which is then indexed by
;;; glyph_item->item->offset to get to the text of glyph_item. The start_index
;;; and end_index values can directly index into text. The start_glyph,
;;; end_glyph, start_char, and end_char values however are zero-based for the
;;; glyph_item. For each cluster, the item pointed at by the start variables is
;;; included in the cluster while the one pointed at by end variables is not.
;;; None of the members of a PangoGlyphItemIter should be modified manually.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_GET_UNKNOWN_GLYPH()
;;;
;;; #define PANGO_GET_UNKNOWN_GLYPH(wc)
;;;         ((PangoGlyph)(wc)|PANGO_GLYPH_UNKNOWN_FLAG)
;;;
;;; Returns a PangoGlyph value that means no glyph was found for wc. The way
;;; this unknown glyphs are rendered is backend specific. For example, a box
;;; with the hexadecimal Unicode code-point of the character written in it is
;;; what is done in the most common backends.
;;;
;;; wc :
;;;     a Unicode character
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_new" %glyph-string-new) :pointer)

(cffi:defcfun ("pango_glyph_string_new" glyph-string-new)
    (g:boxed glyph-string :return)
 #+liber-documentation
 "@version{2026-03-08}
  @return{The newly allocated @class{pango:glyph-string} instance.}
  @short{Creates a new glyph string.}
  @see-class{pango:glyph-string}")

(export 'glyph-string-new)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_copy" glyph-string-copy)
    (g:boxed glyph-string :return)
 #+liber-documentation
 "@version{2026-03-08}
  @argument[glyphs]{a @class{pango:glyph-string} instance, may be nil}
  @begin{return}
    The newly allocated @class{pango:glyph-string} instance, or @code{nil} if
    @arg{glyphs} is @code{nil}.
  @end{return}
  @short{Copies a glyph string and associated storage.}
  @see-class{pango:glyph-string}"
  (glyphs (g:boxed glyph-string)))

(export 'glyph-string-copy)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_set_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_set_size" glyph-string-set-size) :void
 #+liber-documentation
 "@version{2026-03-08}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @argument[len]{an integer for the new length of @arg{glyphs}}
  @begin{short}
    Resize a glyph string to the given length.
  @end{short}
  @see-class{pango:glyph-string}"
  (glyphs (g:boxed glyph-string))
  (len :int))

(export 'glyph-string-set-size)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_free                                 not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_extents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_extents" %glyph-string-extents) :void
  (glyphs (g:boxed glyph-string))
  (font (g:object font))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun glyph-string-extents (glyphs font ink logical)
 #+liber-documentation
 "@version{2026-03-08}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @argument[font]{a @class{pango:font} instance}
  @argument[ink]{a @symbol{pango:rectangle} instance used to store the extents
    of the glyph string as drawn or nil to indicate that the result is not
    needed}
  @argument[logical]{a @symbol{pango:rectangle} instance used to store the
    logical extents of the glyph string or nil to indicate that the result is
    not needed}
  @begin{short}
    Compute the logical and ink extents of a glyph string.
  @end{short}
  See the documentation for the @fun{pango:font-glyph-extents} function for
  details about the interpretation of the rectangles.
  @see-class{pango:glyph-string}
  @see-class{pango:font}
  @see-symbol{pango:rectangle}
  @see-function{pango:font-glyph-extents}"
  (%glyph-string-extents glyphs
                         font
                         (or ink (cffi:null-pointer))
                         (or logical (cffi:null-pointer))))

(export 'glyph-string-extents)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_extents_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_extents_range" %glyph-string-extents-range)
    :void
  (glyphs (g:boxed glyph-string))
  (start :int)
  (end :int)
  (font (g:object font))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun glyph-string-extents-range (glyphs start end font ink logical)
 #+liber-documentation
 "@version{2026-03-08}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @argument[start]{an integer for the start index in bytes}
  @argument[end]{an integer for the end index in bytes}
  @argument[font]{a @class{pango:font} instance}
  @argument[ink]{a @symbol{pango:rectangle} instance used to store the extents
    of the glyph string as drawn or nil to indicate that the result is not
    needed}
  @argument[logical]{a @symbol{pango:rectangle} instance used to store the
    logical extents of the glyph string or nil to indicate that the result is
    not needed}
  @begin{short}
    Computes the extents of a sub-portion of a glyph string.
  @end{short}
  The extents are relative to the start of the glyph string range, the origin
  of their coordinate system is at the start of the range, not at the start of
  the entire glyph string.
  @see-class{pango:glyph-string}
  @see-class{pango:font}
  @see-symbol{pango:rectangle}
  @see-function{pango:glyph-string-extents}"
  (%glyph-string-extents-range glyphs
                               start
                               end
                               font
                               (or ink (cffi:null-pointer))
                               (or logical (cffi:null-pointer))))

(export 'glyph-string-extents-range)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_get_width
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_get_width" glyph-string-width) :int
 #+liber-documentation
 "@version{2026-03-08}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @return{The integer for the logical width of the glyph string.}
  @begin{short}
    Computes the logical width of the glyph string as can also be computed using
    the @fun{pango:glyph-string-extents} function.
  @end{short}
  However, since this only computes the width, it is much faster.
  @see-class{pango:glyph-string}
  @see-function{pango:glyph-string-extents}"
  (glyphs (g:boxed glyph-string)))

(export 'glyph-string-width)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_index_to_x
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_index_to_x" %glyph-string-index-to-x) :void
  (glyphs (g:boxed glyph-string))
  (text :string)
  (len :int)
  (analysis :pointer)
  (index :int)
  (trailing :boolean)
  (xpos (:pointer :int)))

(defun glyph-string-index-to-x (glyphs text len analysis index trailing)
 #+liber-documentation
 "@version{2026-03-08}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @argument[text]{a string for the text of the run}
  @argument[len]{an integer for the number of bytes in @arg{text}}
  @argument[analysis]{a @symbol{pango:analysis} instance for the analysis
    information from the @fun{pango:itemize} function}
  @argument[index]{an integer for the byte index within @arg{text}}
  @argument[trailing]{a boolean whether we should compute the result for the
    beginning (@em{false}) or end (@em{true}) of the character}
  @return{The integer for the result.}
  @begin{short}
    Converts from character position to x position.
  @end{short}

  @image[glyphstring-positions-light]{Figure: Glyph string positions}

  The x position is measured from the left edge of the run. Character positions
  are obtained using font metrics for ligatures where available, and computed by
  dividing up each cluster into equal portions, otherwise.
  @see-class{pango:glyph-string}
  @see-symbol{pango:analysis}
  @see-function{pango:itemize}"
  (cffi:with-foreign-object (xpos :int)
    (%glyph-string-index-to-x glyphs text len analysis index trailing xpos)
    (values (cffi:mem-ref xpos :int))))

(export 'glyph-string-index-to-x)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_index_to_x_full
;;;
;;; Converts from character position to x position.
;;;
;;; Since 1.50
;;; ----------------------------------------------------------------------------

;; TODO: Implementation of PangoLogAttrs is missing for this function.

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_x_to_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_x_to_index" %glyph-string-x-to-index) :void
  (glyphs (g:boxed glyph-string))
  (text :string)
  (len :int)
  (analysis :pointer)
  (xpos :int)
  (index (:pointer :int))
  (trailing (:pointer :boolean)))

(defun glyph-string-x-to-index (glyphs text len analysis xpos)
 #+liber-documentation
 "@version{2026-03-08}
  @syntax{(pango:glyph-string-x-to-index glyphs text len analysis xpos) =>
    index, trailing}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @argument[text]{a string for the text of the run}
  @argument[len]{an integer for the number of bytes in @arg{text}}
  @argument[analysis]{a @symbol{pango:analysis} instance for the analysis
    information from the @fun{pango:itemize} function}
  @argument[xpos]{an integer for the x offset (in Pango units)}
  @argument[index]{an integer for the byte index within @arg{text}}
  @argument[trailing]{a boolean whether the position is on the leading or
    trailing edge of the character}
  @begin{short}
    Convert from x offset to character position.
  @end{short}
  Character positions are computed by dividing up each cluster into equal
  portions. In scripts where positioning within a cluster is not allowed (such
  as Thai), the returned value may not be a valid cursor position. The caller
  must combine the result with the logical attributes for the text to compute
  the valid cursor position.
  @see-class{pango:glyph-string}
  @see-symbol{pango:analysis}
  @see-function{pango:itemize}"
  (cffi:with-foreign-objects ((index :int) (trailing :boolean))
    (%glyph-string-x-to-index glyphs text len analysis xpos index trailing)
    (values (cffi:mem-ref index :int)
            (cffi:mem-ref trailing :boolean))))

(export 'glyph-string-x-to-index)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_get_logical_widths
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_get_logical_widths"
               %glyph-string-logical-widths) :void
  (glyphs (g:boxed glyph-string))
  (text :string)
  (len :int)
  (level :int)
  (widths (:pointer :int)))

(defun glyph-string-logical-widths (glyphs text len level)
 #+liber-documentation
 "@version{2026-03-08}
  @argument[glyphs]{a @class{pango:glyph-string} instance}
  @argument[text]{a string for the text of the run}
  @argument[len]{an integer for the number of bytes in @arg{text}}
  @argument[level]{an integer for hte embedding level of the string}
  @begin{return}
    The array whose length is the number of characters in @arg{text} with the
    resulting character widths.
  @end{return}
  @begin{short}
    Given a glyph string and corresponding @arg{text}, determine the width
    corresponding to each character.
  @end{short}
  When multiple characters compose a single cluster, the width of the entire
  cluster is divided equally among the characters. See also the
  @fun{pango:glyph-item-logical-widths} function.
  @see-class{pango:glyph-string}
  @see-function{pango:glyph-item-logical-widths}"
  (cffi:with-foreign-object (ptr '(:pointer :int) len)
    (%glyph-string-logical-widths glyphs text len level ptr)
    (cffi:foreign-array-to-lisp ptr `(:array :int ,(length text)))))

(export 'glyph-string-logical-widths)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_item_copy" glyph-item-copy)
    (g:boxed glyph-item :return)
 #+liber-documentation
 "@version{2026-03-08}
  @argument[item]{a @class{pango:glyph-item} instance, maybe @code{nil}}
  @begin{return}
    The newly allocated @class{pango:glyph-item} instance, or @code{nil} if
    @arg{item} was @code{nil}.
  @end{return}
  @begin{short}
    Make a deep copy of an existing @class{pango:glyph-item} instance.
  @end{short}
  @see-class{pango:glyph-item}"
  (item (g:boxed glyph-item)))

(export 'glyph-item-copy)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_free                                   not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_split
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_item_split" glyph-item-split)
    (g:boxed glyph-item :return)
 #+liber-documentation
 "@version{#2026-03-23}
  @argument[item]{a @class{pango:glyph-item} instance}
  @argument[text]{a string for the text positions in @arg{item} apply}
  @argument[index]{an integer for the byte index of the position to split
    @arg{item}, relative to the start of the item}
  @begin{return}
    The newly allocated @class{pango:glyph-item} instance representing text
    before @arg{index}.
  @end{return}
  @begin{short}
    Modifies @arg{item} to cover only the text after @arg{index}, and returns a
    new glyph item that covers the text before @arg{index} that used to be in
    @arg{item}.
  @end{short}
  You can think of @arg{index} as the length of the returned item. The
  @arg{index} argument may not be 0, and it may not be greater than or equal to
  the length of @arg{item}, that is, there must be at least one byte assigned
  to each item, you cannot create a zero-length item.

  This function is similar in function to the @fun{pango:item-split} function
  and uses it internally.
  @see-class{pango:glyph-item}
  @see-function{pango:item-split}"
  (item (g:boxed glyph-item))
  (text :string)
  (index :int))

(export 'glyph-item-split)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_apply_attrs ()
;;;
;;; GSList * pango_glyph_item_apply_attrs (PangoGlyphItem *glyph_item,
;;;                                        const char *text,
;;;                                        PangoAttrList *list);
;;;
;;; Splits a shaped item (PangoGlyphItem) into multiple items based on an
;;; attribute list. The idea is that if you have attributes that don't affect
;;; shaping, such as color or underline, to avoid affecting shaping, you filter
;;; them out (pango_attr_list_filter()), apply the shaping process and then
;;; reapply them to the result using this function.
;;;
;;; All attributes that start or end inside a cluster are applied to that
;;; cluster; for instance, if half of a cluster is underlined and the other-half
;;; strikethrough, then the cluster will end up with both underline and
;;; strikethrough attributes. In these cases, it may happen that
;;; item->extra_attrs for some of the result items can have multiple attributes
;;; of the same type.
;;;
;;; This function takes ownership of glyph_item; it will be reused as one of the
;;; elements in the list.
;;;
;;; glyph_item :
;;;     a shaped item
;;;
;;; text :
;;;     text that list applies to
;;;
;;; list :
;;;     a PangoAttrList
;;;
;;; Returns :
;;;     A list of glyph items resulting from splitting glyph_item. Free the
;;;     elements using pango_glyph_item_free(), the list using g_slist_free().
;;; ----------------------------------------------------------------------------

;; TODO: We need pango:attr-list to compile the function. Consider to reorder
;; the files for compilation.

#+nil
(cffi:defcfun ("pango_glyph_item_apply_attrs" glyph-item-apply-attrs)
    (glib:list-t (g:boxed glyph-item :return))
  (item (g:boxed glyph-item))
  (text :string)
  (attrs (g:boxed attr-list)))

#+nil
(export 'glyph-item-apply-attrs)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_letter_space ()
;;;
;;; void pango_glyph_item_letter_space (PangoGlyphItem *glyph_item,
;;;                                     const char *text,
;;;                                     PangoLogAttr *log_attrs,
;;;                                     int letter_spacing);
;;;
;;; Adds spacing between the graphemes of glyph_item to give the effect of
;;; typographic letter spacing.
;;;
;;; glyph_item :
;;;     a PangoGlyphItem
;;;
;;; text :
;;;     text that glyph_item corresponds to (glyph_item->item->offset is an
;;;     offset from the start of text)
;;;
;;; log_attrs :
;;;     logical attributes for the item (the first logical attribute refers to
;;;     the position before the first character in the item)
;;;
;;; letter_spacing :
;;;     amount of letter spacing to add in Pango units. May be negative, though
;;;     too large negative values will give ugly results.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_get_logical_widths ()
;;;
;;; void pango_glyph_item_get_logical_widths (PangoGlyphItem *glyph_item,
;;;                                           const char *text,
;;;                                           int *logical_widths);
;;;
;;; Given a PangoGlyphItem and the corresponding text, determine the screen
;;; width corresponding to each character. When multiple characters compose a
;;; single cluster, the width of the entire cluster is divided equally among the
;;; characters.
;;;
;;; See also pango_glyph_string_get_logical_widths().
;;;
;;; glyph_item :
;;;     a PangoGlyphItem
;;;
;;; text :
;;;     text that glyph_item corresponds to (glyph_item->item->offset is an
;;;     offset from the start of text)
;;;
;;; logical_widths :
;;;     an array whose length is the number of characters in glyph_item (equal
;;;     to glyph_item->item->num_chars) to be filled in with the resulting
;;;     character widths
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_copy ()
;;;
;;; PangoGlyphItemIter * pango_glyph_item_iter_copy (PangoGlyphItemIter *orig);
;;;
;;; Make a shallow copy of an existing PangoGlyphItemIter structure.
;;;
;;; orig :
;;;     a PangoGlyphItemIter, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoGlyphItemIter, which should be freed with
;;;     pango_glyph_item_iter_free(), or NULL if orig was NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_free ()
;;;
;;; void pango_glyph_item_iter_free (PangoGlyphItemIter *iter);
;;;
;;; Frees a PangoGlyphItemIter created by pango_glyph_item_iter_copy().
;;;
;;; iter :
;;;     a PangoGlyphItemIter, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_init_start ()
;;;
;;; gboolean pango_glyph_item_iter_init_start (PangoGlyphItemIter *iter,
;;;                                            PangoGlyphItem *glyph_item,
;;;                                            const char *text);
;;;
;;; Initializes a PangoGlyphItemIter structure to point to the first cluster in
;;; a glyph item. See PangoGlyphItemIter for details of cluster orders.
;;;
;;; iter :
;;;     a PangoGlyphItemIter
;;;
;;; glyph_item :
;;;     the glyph item to iterate over
;;;
;;; text :
;;;     text corresponding to the glyph item
;;;
;;; Returns :
;;;     FALSE if there are no clusters in the glyph item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_init_end ()
;;;
;;; gboolean pango_glyph_item_iter_init_end (PangoGlyphItemIter *iter,
;;;                                          PangoGlyphItem *glyph_item,
;;;                                          const char *text);
;;;
;;; Initializes a PangoGlyphItemIter structure to point to the last cluster in
;;; a glyph item. See PangoGlyphItemIter for details of cluster orders.
;;;
;;; iter :
;;;     a PangoGlyphItemIter
;;;
;;; glyph_item :
;;;     the glyph item to iterate over
;;;
;;; text :
;;;     text corresponding to the glyph item
;;;
;;; Returns :
;;;     FALSE if there are no clusters in the glyph item
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_next_cluster ()
;;;
;;; gboolean pango_glyph_item_iter_next_cluster (PangoGlyphItemIter *iter);
;;;
;;; Advances the iterator to the next cluster in the glyph item. See
;;; PangoGlyphItemIter for details of cluster orders.
;;;
;;; iter :
;;;     a PangoGlyphItemIter
;;;
;;; Returns :
;;;     TRUE if the iterator was advanced, FALSE if we were already on the last
;;;     cluster.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_iter_prev_cluster ()
;;;
;;; gboolean pango_glyph_item_iter_prev_cluster (PangoGlyphItemIter *iter);
;;;
;;; Moves the iterator to the preceding cluster in the glyph item. See
;;; PangoGlyphItemIter for details of cluster orders.
;;;
;;; iter :
;;;     a PangoGlyphItemIter
;;;
;;; Returns :
;;;     TRUE if the iterator was moved, FALSE if we were already on the first
;;;     cluster.
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.glyph.lisp -------------------------------------------
