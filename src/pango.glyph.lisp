;;; ----------------------------------------------------------------------------
;;; pango.glyph.lisp
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
;;;     pango_glyph_string_free
;;;     pango_glyph_string_extents
;;;     pango_glyph_string_extents_range
;;;     pango_glyph_string_get_width
;;;     pango_glyph_string_index_to_x
;;;     pango_glyph_string_index_to_x_full                 Since 1.50
;;;     pango_glyph_string_x_to_index
;;;     pango_glyph_string_get_logical_widths
;;;
;;;     pango_glyph_item_copy
;;;     pango_glyph_item_free
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
 "@version{2025-08-24}
  @begin{short}
    A @sym{pango:glyph} type represents a single glyph in the output form of
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
 "@version{2024-03-06}
  @begin{short}
    The @class{pango:glyph-string} structure is used to store strings of glyphs
    with geometry and visual attribute information.
  @end{short}
  The storage for the glyph information is owned by the structure which
  simplifies memory management.
  @see-class{pango:glyph-item}")

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
 "@version{2024-03-06}
  @begin{short}
    A @class{pango:glyph-item} structure is a pair of a @class{pango:item}
    instance and the glyphs resulting from shaping the text corresponding to
    an item.
  @end{short}
  As an example of the usage of the @class{pango:glyph-item} structure, the
  results of shaping text with the @class{pango:layout} class is a list of
  @class{pango:layout-line} objects, each of which contains a list of
  @class{pango:glyph-item} instances.
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
;;; pango_glyph_string_new ()
;;;
;;; PangoGlyphString * pango_glyph_string_new (void);
;;;
;;; Create a new PangoGlyphString.
;;;
;;; Returns :
;;;     the newly allocated PangoGlyphString, which should be freed with
;;;     pango_glyph_string_free().
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_new" %glyph-string-new) :pointer)

(cffi:defcfun ("pango_glyph_string_new" glyph-string-new)
    (g:boxed glyph-string :return))

(export 'glyph-string-new)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_copy ()
;;;
;;; PangoGlyphString * pango_glyph_string_copy (PangoGlyphString *string);
;;;
;;; Copy a glyph string and associated storage.
;;;
;;; string :
;;;     a PangoGlyphString, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoGlyphString, which should be freed with
;;;     pango_glyph_string_free(), or NULL if string was NULL.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_copy" glyph-string-copy)
    (g:boxed glyph-string :return)
  (string (g:boxed glyph-string)))

(export 'glyph-string-copy)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_set_size ()
;;;
;;; void pango_glyph_string_set_size (PangoGlyphString *string, gint new_len);
;;;
;;; Resize a glyph string to the given length.
;;;
;;; string :
;;;     a PangoGlyphString.
;;;
;;; new_len :
;;;     the new length of the string.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_set_size" glyph-string-set-size) :void
  (string (g:boxed glyph-string))
  (len :int))

(export 'glyph-string-set-size)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_free ()
;;;
;;; void pango_glyph_string_free (PangoGlyphString *string);
;;;
;;; Free a glyph string and associated storage.
;;;
;;; string :
;;;     a PangoGlyphString, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_extents ()
;;;
;;; void pango_glyph_string_extents (PangoGlyphString *glyphs,
;;;                                  PangoFont *font,
;;;                                  PangoRectangle *ink_rect,
;;;                                  PangoRectangle *logical_rect);
;;;
;;; Compute the logical and ink extents of a glyph string. See the documentation
;;; for pango_font_get_glyph_extents() for details about the interpretation of
;;; the rectangles.
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; font :
;;;     a PangoFont
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the glyph string as drawn or NULL
;;;     to indicate that the result is not needed
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the glyph string or NULL
;;;     to indicate that the result is not needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_extents" %glyph-string-extents) :void
  (glyph (g:boxed glyph-string))
  (font (g:object font))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun glyph-string-extents (glyph font ink logical)
  (%glyph-string-extents glyph
                         font
                         (or ink (cffi:null-pointer))
                         (or logical (cffi:null-pointer))))

(export 'glyph-string-extents)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_extents_range ()
;;;
;;; void pango_glyph_string_extents_range (PangoGlyphString *glyphs,
;;;                                        int start,
;;;                                        int end,
;;;                                        PangoFont *font,
;;;                                        PangoRectangle *ink_rect,
;;;                                        PangoRectangle *logical_rect);
;;;
;;; Computes the extents of a sub-portion of a glyph string. The extents are
;;; relative to the start of the glyph string range (the origin of their
;;; coordinate system is at the start of the range, not at the start of the
;;; entire glyph string).
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; start :
;;;     start index
;;;
;;; end :
;;;     end index (the range is the set of bytes with indices such that
;;;     start <= index < end)
;;;
;;; font :
;;;     a PangoFont
;;;
;;; ink_rect :
;;;     rectangle used to store the extents of the glyph string range as drawn
;;;     or NULL to indicate that the result is not needed
;;;
;;; logical_rect :
;;;     rectangle used to store the logical extents of the glyph string range or
;;;     NULL to indicate that the result is not needed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_extents-range" %glyph-string-extents-range)
    :void
  (glyph (g:boxed glyph-string))
  (start :int)
  (end :int)
  (font (g:object font))
  (ink (:pointer (:struct rectangle)))
  (logical (:pointer (:struct rectangle))))

(defun glyph-string-extents-range (glyph start end font ink logical)
  (%glyph-string-extents-range glyph
                               start
                               end
                               font
                               (or ink (cffi:null-pointer))
                               (or logical (cffi:null-pointer))))

(export 'glyph-string-extents-range)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_get_width ()
;;;
;;; int pango_glyph_string_get_width (PangoGlyphString *glyphs);
;;;
;;; Computes the logical width of the glyph string as can also be computed using
;;; pango_glyph_string_extents(). However, since this only computes the width,
;;; it's much faster. This is in fact only a convenience function that computes
;;; the sum of geometry.width for each glyph in the glyphs.
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; Returns :
;;;     the logical width of the glyph string.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_get_width" glyph-string-width) :int
  (glyphs (g:boxed glyph-string)))

(export 'glyph-string-width)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_index_to_x ()
;;;
;;; void pango_glyph_string_index_to_x (PangoGlyphString *glyphs,
;;;                                     char *text,
;;;                                     int length,
;;;                                     PangoAnalysis *analysis,
;;;                                     int index_,
;;;                                     gboolean trailing,
;;;                                     int *x_pos);
;;;
;;; Converts from character position to x position. (X position is measured from
;;; the left edge of the run). Character positions are computed by dividing up
;;; each cluster into equal portions.
;;;
;;; glyphs :
;;;     the glyphs return from pango_shape()
;;;
;;; text :
;;;     the text for the run
;;;
;;; length :
;;;     the number of bytes (not characters) in text.
;;;
;;; analysis :
;;;     the analysis information return from pango_itemize()
;;;
;;; index_ :
;;;     the byte index within text
;;;
;;; trailing :
;;;     whether we should compute the result for the beginning (FALSE) or end
;;;     (TRUE) of the character
;;;
;;; x_pos :
;;;     location to store result
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

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_x_to_index ()
;;;
;;; void pango_glyph_string_x_to_index (PangoGlyphString *glyphs,
;;;                                     char *text,
;;;                                     int length,
;;;                                     PangoAnalysis *analysis,
;;;                                     int x_pos,
;;;                                     int *index_,
;;;                                     int *trailing);
;;;
;;; Convert from x offset to character position. Character positions are
;;; computed by dividing up each cluster into equal portions. In scripts where
;;; positioning within a cluster is not allowed (such as Thai), the returned
;;; value may not be a valid cursor position; the caller must combine the result
;;; with the logical attributes for the text to compute the valid cursor
;;; position.
;;;
;;; glyphs :
;;;     the glyphs returned from pango_shape()
;;;
;;; text :
;;;     the text for the run
;;;
;;; length :
;;;     the number of bytes (not characters) in text.
;;;
;;; analysis :
;;;     the analysis information return from pango_itemize()
;;;
;;; x_pos :
;;;     the x offset (in Pango units)
;;;
;;; index_ :
;;;     location to store calculated byte index within text
;;;
;;; trailing :
;;;     location to store a boolean indicating whether the user clicked on the
;;;     leading or trailing edge of the character
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_string_x_to_index" %glyph-string-x-to-index) :void
  (glyphs (g:boxed glyph-string))
  (text :string)
  (len :int)
  (analysis :pointer)
  (xpos :int)
  (index (:pointer :int))
  (trailing :boolean))

(defun glyph-string-x-to-index (glyphs text len analysis xpos trailing)
  (cffi:with-foreign-object (index :int)
    (%glyph-string-x-to-index glyphs text len analysis xpos index trailing)
    (values (cffi:mem-ref index :int))))

(export 'glyph-string-x-to-index)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_string_get_logical_widths ()
;;;
;;; void pango_glyph_string_get_logical_widths (PangoGlyphString *glyphs,
;;;                                             const char *text,
;;;                                             int length,
;;;                                             int embedding_level,
;;;                                             int *logical_widths);
;;;
;;; Given a PangoGlyphString resulting from pango_shape() and the corresponding
;;; text, determine the screen width corresponding to each character. When
;;; multiple characters compose a single cluster, the width of the entire
;;; cluster is divided equally among the characters.
;;;
;;; See also pango_glyph_item_get_logical_widths().
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; text :
;;;     the text corresponding to the glyphs
;;;
;;; length :
;;;     the length of text, in bytes
;;;
;;; embedding_level :
;;;     the embedding level of the string
;;;
;;; logical_widths :
;;;     an array whose length is the number of characters in text (equal to
;;;     g_utf8_strlen (text, length) unless text has NUL bytes) to be filled in
;;;     with the resulting character widths
;;; ----------------------------------------------------------------------------

;; TODO: Finish the implementation

(cffi:defcfun ("pango_glyph_string_get_logical_widths"
               glyph-string-logical-widths) :void
  (glyphs (g:boxed glyph-string))
  (text :string)
  (len :int)
  (level :int)
  (widths :pointer))

(export 'glyph-string-logical-widths)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_copy ()
;;;
;;; PangoGlyphItem * pango_glyph_item_copy (PangoGlyphItem *orig);
;;;
;;; Make a deep copy of an existing PangoGlyphItem structure.
;;;
;;; orig :
;;;     a PangoGlyphItem, may be NULL
;;;
;;; Returns :
;;;     the newly allocated PangoGlyphItem, which should be freed with
;;;     pango_glyph_item_free(), or NULL if orig was NULL.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_item_copy" glyph-item-copy)
    (g:boxed glyph-item :return)
  (item (g:boxed glyph-item)))

(export 'glyph-item-copy)

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_free ()
;;;
;;; void pango_glyph_item_free (PangoGlyphItem *glyph_item);
;;;
;;; Frees a PangoGlyphItem and resources to which it points.
;;;
;;; glyph_item :
;;;     a PangoGlyphItem, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_glyph_item_split ()
;;;
;;; PangoGlyphItem * pango_glyph_item_split (PangoGlyphItem *orig,
;;;                                          const char *text,
;;;                                          int split_index);
;;;
;;; Modifies orig to cover only the text after split_index, and returns a new
;;; item that covers the text before split_index that used to be in orig. You
;;; can think of split_index as the length of the returned item. split_index may
;;; not be 0, and it may not be greater than or equal to the length of orig
;;; (that is, there must be at least one byte assigned to each item, you can't
;;; create a zero-length item).
;;;
;;; This function is similar in function to pango_item_split() (and uses it
;;; internally.)
;;;
;;; orig :
;;;     a PangoItem
;;;
;;; text :
;;;     text to which positions in orig apply
;;;
;;; split_index :
;;;     byte index of position to split item, relative to the start of the item
;;;
;;; Returns :
;;;     the newly allocated item representing text before split_index, which
;;;     should be freed with pango_glyph_item_free().
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_glyph_item_split" glyph-item-split)
    (g:boxed glyph-item :return)
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
