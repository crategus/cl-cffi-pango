;;; ----------------------------------------------------------------------------
;;; pango.rendering.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.50 and modified to document the Lisp binding to the Pango library.
;;; See <http://www.pango.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2020 - 2023 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; Rendering
;;;
;;;     Functions to run the rendering pipeline
;;;
;;; Types and Values
;;;
;;;     PangoItem
;;;     PangoAnalysis
;;;
;;;     PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;     PANGO_ANALYSIS_FLAG_IS_ELLIPSIS
;;;     PANGO_ANALYSIS_FLAG_NEED_HYPHEN
;;;
;;;     PangoLogAttr
;;;     PangoShapeFlags
;;;
;;; Functions
;;;
;;;     pango_itemize
;;;     pango_itemize_with_base_dir
;;;     pango_item_free
;;;     pango_item_copy
;;;     pango_item_new
;;;     pango_item_split
;;;     pango_item_apply_attrs
;;;     pango_reorder_items
;;;     pango_break
;;;     pango_get_log_attrs
;;;     pango_find_paragraph_boundary
;;;     pango_default_break
;;;     pango_tailor_break
;;;     pango_shape
;;;     pango_shape_full
;;;     pango_shape_with_flags
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ????????? PangoItem
;;;
;;;     GFlags
;;;     ????????? PangoShapeFlags
;;;
;;; Description
;;;
;;;     The Pango rendering pipeline takes a string of Unicode characters and
;;;     converts it into glyphs. The functions described in this section
;;;     accomplish various steps of this process.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; struct PangoAnalysis
;;; ----------------------------------------------------------------------------

(defcstruct analysis
  (shape-engine :pointer)
  (lang-engine :pointer)
  (font (g:object font))
  (level :uint8)
  (gravity :uint8)
  (flags :uint8)
  (script :uint8)
  (language (g:boxed language))
  (extra-attrs :pointer))

#+liber-documentation
(setf (liber:alias-for-symbol 'analysis)
      "CStruct"
      (liber:symbol-documentation 'analysis)
 "@version{#2021-1-11}
  @begin{short}
    The @sym{pango:analysis} structure stores information about the properties
    of a segment of text.
  @end{short}
  @begin{pre}
(defcstruct analysis
  (shape-engine :pointer)
  (lang-engine :pointer)
  (font (g:object font))
  (level :uint8)
  (gravity :uint8)
  (flags :uint8)
  (script :uint8)
  (language (g:boxed language))
  (extra-attrs :pointer))
  @end{pre}
  @begin[code]{table}
    @entry[shape-engine]{Unused.}
    @entry[lang-engine]{Unused.}
    @entry[font]{The font for this segment.}
    @entry[level]{The bidirectional level for this segment.}
    @entry[gravity]{The glyph orientation for this segment (A PangoGravity).}
    @entry[flags]{Boolean flags for this segment.}
    @entry[script]{The detected script for this segment (A PangoScript).}
    @entry[language]{The detected language for this segment.}
    @entry[extra-attrs]{Extra attributes for this segment.}
  @end{table}
  @see-class{pango:item}
  @see-class{pango:font}
  @see-class{pango:language}")

(export 'analysis)

;;; ----------------------------------------------------------------------------
;;; struct PangoItem
;;; ----------------------------------------------------------------------------

(define-g-boxed-cstruct item "PangoItem"
  (:export t
   :type-initializer "pango_item_get_type")
  (offset :int :initform 0)
  (length :int :initform 0)
  (num-chars :int :initform 0)
  (analysis (:pointer (:struct analysis)) :initform (cffi:null-pointer)))

#+liber-documentation
(setf (liber:alias-for-class 'item)
      "GBoxed"
      (documentation 'item 'type)
 "@version{#2023-2-5}
  @begin{short}
    The @sym{pango:item} structure stores information about a segment of text.
  @end{short}
  @begin{pre}
(define-g-boxed-cstruct item \"PangoItem\"
  (:export t
   :type-initializer \"pango_item_get_type\")
  (offset :int :initform 0)
  (length :int :initform 0)
  (num-chars :int :initform 0)
  (analysis (:pointer (:struct analysis)) :initform (cffi:null-pointer)))
  @end{pre}
  @begin[code]{table}
    @entry[offset]{Byte offset of the start of this item in text.}
    @entry[length]{Length of this item in bytes.}
    @entry[num-chars]{Number of Unicode characters in the item.}
    @entry[analysis]{Analysis results for the item.}
  @end{table}
  @see-slot{pango:item-offset}
  @see-slot{pango:item-length}
  @see-slot{pango:item-num-chars}
  @see-slot{pango:item-analysis}
  @see-symbol{pango:analysis}")

;;; ----------------------------------------------------------------------------
;;; PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;
;;; #define PANGO_ANALYSIS_FLAG_CENTERED_BASELINE (1 << 0)
;;;
;;; Whether the segment should be shifted to center around the baseline. Used
;;; in vertical writing directions mostly.
;;;
;;; Since 1.16
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ANALYSIS_FLAG_IS_ELLIPSIS
;;;
;;; #define PANGO_ANALYSIS_FLAG_IS_ELLIPSIS (1 << 1)
;;;
;;; This flag is used to mark runs that hold ellipsized text, in an ellipsized
;;; layout.
;;;
;;; Since 1.36
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PANGO_ANALYSIS_FLAG_NEED_HYPHEN
;;;
;;; #define PANGO_ANALYSIS_FLAG_NEED_HYPHEN (1 << 2)
;;;
;;; This flag tells Pango to add a hyphen at the end of the run during shaping.
;;;
;;; Since 1.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; PangoLogAttr
;;; ----------------------------------------------------------------------------

(defcstruct log-attr
  (is-line-break :uint)
  (is-mandatory-break :uint)
  (is-char-break :uint)
  (is-white :uint)
  (is-cursor-position :uint)
  (is-word-start :uint)
  (is-word-end :uint)
  (is-sentence-boundary :uint)
  (is-sentence-start :uint)
  (is-sentence-end :uint)
  (backspcaces-deletes-character :uint)
  (is-expandable-space :uint)
  (is-word-boundary :uint))

#+liber-documentation
(setf (liber:alias-for-symbol 'log-attr)
      "CStruct"
      (liber:symbol-documentation 'log-attr)
 "@version{#2021-1-6}
  @begin{short}
    The @sym{pango:log-attr} structure stores information about the attributes
    of a single character.
  @end{short}
  @begin{pre}
(defcstruct log-attr
  (is-line-break :uint)
  (is-mandatory-break :uint)
  (is-char-break :uint)
  (is-white :uint)
  (is-cursor-position :uint)
  (is-word-start :uint)
  (is-word-end :uint)
  (is-sentence-boundary :uint)
  (is-sentence-start :uint)
  (is-sentence-end :uint)
  (backspcaces-deletes-character :uint)
  (is-expandable-space :uint)
  (is-word-boundary :uint))
  @end{pre}
  @begin[code]{table}
    @entry[is-line-break]{If set, can break line in front of character.}
    @entry[is-mandatory-break]{If set, must break line in front of character.}
    @entry[is-char-break]{If set, can break here when doing character wrapping.}
    @entry[is-white]{Is whitespace character.}
    @entry[is-cursor-position]{If set, cursor can appear in front of character.
      I.e. this is a grapheme boundary, or the first character in the text.
      This flag implements Unicode's Grapheme Cluster Boundaries semantics.}
    @entry[is-word-start]{Is first character in a word.}
    @entry[is-word-end]{Is first non-word char after a word. Note that in
      degenerate cases, you could have both @code{is-word-start} and
      @code{is-word-end} set for some character.}
    @entry[is-sentence-boundary]{Is a sentence boundary. There are two ways to
      divide sentences. The first assigns all inter-sentence
      whitespace/control/format chars to some sentence, so all chars are in
      some sentence; @code{is-sentence-boundary} denotes the boundaries there.
      The second way does not assign between-sentence spaces, etc. to any
      sentence, so @code{is-sentence_start}/@code{is-sentence-end} mark the
      boundaries of those sentences.}
    @entry[is-sentence-start]{Is first character in a sentence.}
    @entry[is-sentence-end]{Is first char after a sentence. Note that in
      degenerate cases, you could have both @code{is-sentence-start} and
      @code{is-sentence-end} set for some character, e.g. no space after a
      period, so the next sentence starts right away.}
    @entry[backspace-deletes-character]{If set, backspace deletes one character
      rather than the entire grapheme cluster. This field is only meaningful on
      grapheme boundaries, where @code{is-cursor-position} is set. In some
      languages, the full grapheme, e.g. letter + diacritics, is considered a
      unit, while in others, each decomposed character in the grapheme is a
      unit. In the default implementation of @fun{pango:break}, this bit is set
      on all grapheme boundaries except those following Latin, Cyrillic or
      Greek base characters.}
    @entry[is-expandable-space]{Is a whitespace character that can possibly be
      expanded for justification purposes.}
    @entry[is-word-boundary]{Is a word boundary. More specifically, means that
      this is not a position in the middle of a word. For example, both sides
      of a punctuation mark are considered word boundaries. This flag is
      particularly useful when selecting text word-by-word. This flag
      implements Unicode's Word Boundaries semantics.}
  @end{table}
  @see-class{pango:layout}
  @see-function{pango:break}")

(export 'log-attr)

;;; ----------------------------------------------------------------------------
;;; enum PangoShapeFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "PangoShapeFlags" shape-flags
  (:export t
   :type-initializer "pango_shape_flags_get_type")
  (:none 0)
  (:round-positions #.(ash 1 0)))

#+liber-documentation
(setf (liber:alias-for-symbol 'shape-flags)
      "GFlags"
      (liber:symbol-documentation 'shape-flags)
 "@version{#2021-8-16}
  @begin{short}
    Flags influencing the shaping process.
  @end{short}
  These can be passed to the function @fun{pango:shape-with-flags}.
  @begin{pre}
(define-g-flags \"PangoShapeFlags\" shape-flags
  (:export t
   :type-initializer \"pango_shape_flags_get_type\")
  (:none 0)
  (:round-positions #.(ash 1 0)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Default value.}
    @entry[:round-positions]{Round glyph positions and widths to whole device
      units. This option should be set if the target renderer cannot do subpixel
      positioning of glyphs.}
  @end{table}
  @see-function{pango:shape-with-flags}")

;;; ----------------------------------------------------------------------------
;;; pango_itemize ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_itemize" itemize)
    (g:list-t (g:boxed item))
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[context]{a @class{pango:context} object holding information that
    affects the itemization process}
  @argument[text]{a string with the text to itemize, must be valid UTF-8}
  @argument[start]{an integer with the first byte in @arg{text} to process}
  @argument[length]{an integer with the number of bytes (not characters) to
    process after @arg{start}, this must be >= 0}
  @argument[attrs]{the @class{pango:attr-list} set of attributes that apply to
    @arg{text}}
  @argument[iter]{cached @class{pango:attr-iterator} attribute iterator, or
    @code{nil}}
  @return{A list of @class{pango:item} instances.}
  @begin{short}
    Breaks a piece of text into segments with consistent directional level and
    shaping engine.
  @end{short}
  Each byte of @arg{text} will be contained in exactly one of the items in the
  returned list. The generated list of items will be in logical order, the
  start offsets of the items are ascending.

  The argument @arg{iter} should be an iterator over @arg{attrs} currently
  positioned at a range before or containing @arg{start}. The argument
  @arg{iter} will be advanced to the range covering the position just after
  @arg{start} + @arg{length}, i.e. if itemizing in a loop, just keep passing
  in the same @arg{iter}.
  @see-class{pango:context}
  @see-class{pango:item}
  @see-class{pango:attr-list}
  @see-class{pango:attr-iterator}"
  (context (g:object context))
  (text :string)
  (start :int)
  (length :int)
  (attrs (g:boxed attr-list))
  (iter (g:boxed attr-iterator)))

(export 'itemize)

;;; ----------------------------------------------------------------------------
;;; pango_itemize_with_base_dir ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_itemize_with_base_dir" itemize-with-base-dir)
    (g:list-t (g:boxed item))
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[context]{a @class{pango:context} object holding information that
    affects the itemization process}
  @argument[base-dir]{a @symbol{pango:direction} value with the base direction
    to use for bidirectional processing}
  @argument[text]{a string with the text to itemize, must be valid UTF-8}
  @argument[start]{an integer with the first byte in @arg{text} to process}
  @argument[length]{an integer with the number of bytes (not characters) to
    process after @arg{start}, this must be >= 0}
  @argument[attrs]{the @class{pango:attr-list} set of attributes that apply to
    @arg{text}}
  @argument[iter]{cached @class{pango:attr-iterator} attribute iterator,
     or @code{nil}}
  @begin{return}
    A list of @class{pango:item} instances.
  @end{return}
  @begin{short}
    Like the function @fun{pango:itemize}, but the base direction to use when
    computing bidirectional levels, see the function
    @fun{pango:context-base-dir}, is specified explicitly rather than gotten
    from the @class{pango:context} object.
  @end{short}
  @see-class{pango:context}
  @see-class{pango:item}
  @see-class{pango:attr-list}
  @see-class{pango:attr-iterator}
  @see-symbol{pango:direction}
  @see-function{pango:itemize}
  @see-function{pango:context-base-dir}"
  (context (g:object context))
  (base-dir direction)
  (text :string)
  (start :int)
  (length :int)
  (attrs (g:boxed attr-list))
  (iter (g:boxed attr-iterator)))

(export 'itemize-with-base-dir)

;;; ----------------------------------------------------------------------------
;;; pango_item_free ()
;;;
;;; void
;;; pango_item_free (PangoItem *item);
;;;
;;; Free a PangoItem and all associated memory.
;;;
;;; item :
;;;     a PangoItem, may be NULL.
;;; ----------------------------------------------------------------------------

(defcfun ("pango_item_free" item-free) :void
  (item (g:boxed item)))

;;; ----------------------------------------------------------------------------
;;; pango_item_copy ()
;;; ----------------------------------------------------------------------------

(defun item-copy (item)
 #+liber-documentation
 "@version{#2021-1-11}
  @argument[item]{a @class{pango:item} instance, may be @code{nil}}
  @begin{return}
    The newly allocated @class{pango:item} object, or @code{nil} if @arg{item}
    was @code{nil}.
  @end{return}
  @begin{short}
    Copy an existing @class{pango:item} structure.
  @end{short}
  @see-class{pango:item}"
  (copy-item item))

(export 'item-copy)

;;; ----------------------------------------------------------------------------
;;; pango_item_new ()
;;; ----------------------------------------------------------------------------

(defun item-new ()
 #+liber-documentation
 "@version{#2021-1-11}
  @begin{return}
    The newly allocated @class{pango:item} structure.
  @end{return}
  @begin{short}
    Creates a new @class{pango:item} structure initialized to default values.
  @end{short}
  @see-class{pango:item}"
  (make-item))

(export 'item-new)

;;; ----------------------------------------------------------------------------
;;; pango_item_split ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_item_split" item-split) (g:boxed item)

 #+liber-documentation
 "@version{#2021-01-12}
  @argument[orig]{a @class{pango:item} instance}
  @argument[split-index]{an integer with the byte index of position to split
    item, relative to the start of the item}
  @argument[split-offset]{an integer with the number of chars between start of
    @arg{orig} and @arg{split-index}}
  @begin{return}
    New @class{pango:item} instance representing text before @arg{split-index}.
  @end{return}
  @begin{short}
    Modifies @arg{orig} to cover only the text after @arg{split-index}, and
    returns a new item that covers the text before @arg{split-index} that used
    to be in @arg{orig}.
  @end{short}
  You can think of @arg{split-index} as the length of the returned item.
  @arg{split-index} may not be 0, and it may not be greater than or equal to the
  length of @arg{orig} (that is, there must be at least one byte assigned to
  each item, you cannot create a zero-length item). @arg{split-offset} is the
  length of the first item in chars, and must be provided because the text used
  to generate the item is not available, so the function @sym{pango:item-split}
  cannot count the char length of the split items itself.
  @see-class{pango:item}"
  (orig (g:boxed item))
  (split-index :int)
  (split-offset :int))

(export 'item-split)

;;; ----------------------------------------------------------------------------
;;; pango_item_apply_attrs ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defcfun ("pango_item_apply_attrs" item-apply-attrs) :void
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[item]{a @class{pango:item} instance}
  @argument[iter]{a @class{pango:attr-iterator} instance}
  @begin{short}
    Add attributes to a @class{pango:item} instance.
  @end{short}
  The idea is that you have attributes that do not affect itemization, such as
  font features, so you filter them out using the function
  @fun{pango:attr-list-filter}, itemize your text, then reapply the attributes
  to the resulting items using this function.

  The @arg{iter} should be positioned before the range of the item, and will be
  advanced past it. This function is meant to be called in a loop over the items
  resulting from itemization, while passing the iter to each call.

  Since 1.44
  @see-class{pango:item}
  @see-class{pango:attr-iterator}
  @see-function{pango:attr-list-filter}"
  (item (g:boxed item))
  (iter (g:boxed attr-iterator)))

#+pango-1-44
(export 'item-apply-attrs)

;;; ----------------------------------------------------------------------------
;;; pango_reorder_items ()
;;;
;;; GList *
;;; pango_reorder_items (GList *logical_items);
;;;
;;; From a list of items in logical order and the associated directional
;;; levels, produce a list in visual order. The original list is unmodified.
;;;
;;; logical_items :
;;;     a GList of PangoItem in logical order.
;;;
;;; Returns :
;;;     a GList of PangoItem structures in visual order.
;;;
;;; (Please open a bug if you use this function. It is not a particularly
;;; convenient interface, and the code is duplicated elsewhere in Pango for
;;; that reason.).
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_break ()                                         not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defcfun ("pango_break" standard-break) :void
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[text]{a string with the text to process, must be valid UTF-8}
  @argument[length]{an integer with the length of @arg{text} in bytes (may be
    -1 if text is @code{nul}-terminated)}
  @argument[analysis]{a @symbol{pango:analysis} structure from the function
    @fun{pango:itemize}}
  @argument[attrs]{an array to store character information in}
  @argument[attrs-len]{an integer with the size of the array passed as
    @arg{attrs}}
  @begin{short}
    Determines possible line, word, and character breaks for a string of
    Unicode text with a single analysis.
  @end{short}
  For most purposes you may want to use the function @fun{pango:log-attrs}.
  @begin[Warning]{dictionary}
    The @sym{pango:standard-break} function has been deprecated since version
    1.44 and should not be used in newly written code. Use the
    @fun{pango:default-break} and @fun{pango:tailor-break} functions.
  @end{dictionary}
  @see-symbol{pango:analysis}
  @see-symbol{pango:log-attr}
  @see-function{pango:itemize}
  @see-function{pango:log-attrs}
  @see-function{pango:default-break}
  @see-function{pango:tailor-break}"
  (text :string)
  (length :int)
  (analysis (:pointer (:struct analysis)))
  (attrs (:pointer (:struct log-attr)))
  (attrs-len :int))

#+nil
(export 'standard-break)

;;; ----------------------------------------------------------------------------
;;; pango_get_log_attrs ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_log_attrs" log-attrs) :void
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[text]{a string with the text to process, must be valid UTF-8}
  @argument[length]{an integer with the length in bytes of @arg{text}}
  @argument[level]{an integer with the embedding level, or -1 if unknown}
  @argument[language]{a @class{pango:language} tag}
  @argument[log-attrs]{array with one @symbol{pango:log-attr} instance per
    character in @arg{text}, plus one extra, to be filled in}
  @argument[attrs-len]{an integer with the length of @arg{log-attrs} array}
  @begin{short}
    Computes a @symbol{pango:log-attr} instance for each character in
    @arg{text}.
  @end{short}
  The @arg{log-attrs} array must have one @symbol{pango:log-attr} instance for
  each position in @arg{text}. If @arg{text} contains N characters, it has N+1
  positions, including the last position at the end of the text. text should be
  an entire paragraph; logical attributes cannot be computed without context
  (for example you need to see spaces on either side of a word to know the word
  is a word).
  @see-class{pango:language}
  @see-symbol{pango:log-attr}"
  (text :string)
  (length :int)
  (level :int)
  (language (g:boxed language))
  (log-attrs (:pointer (:struct log-attr)))
  (attrs-len :int))

(export 'log-attrs)

;;; ----------------------------------------------------------------------------
;;; pango_find_paragraph_boundary ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_find_paragraph_boundary" find-paragraph-boundary) :void
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[text]{a string with the UTF-8 text}
  @argument[length]{an integer with the length of @arg{text} in bytes, or -1
    if @code{nul}-terminated}
  @argument[paragraph-delimiter-index]{return location for index of delimiter}
  @argument[next-paragraph-start]{return location for start of next paragraph}
  @begin{short}
    Locates a paragraph boundary in @arg{text}.
  @end{short}
  A boundary is caused by delimiter characters, such as a newline, carriage
  return, carriage return-newline pair, or Unicode paragraph separator
  character. The index of the run of delimiters is returned in
  @arg{paragraph-delimiter-index}. The index of the start of the paragraph
  (index after all delimiters) is stored in @arg{next-paragraph-start}.

  If no delimiters are found, both @arg{paragraph-delimiter-index} and
  @arg{next-paragraph-start} are filled with the length of @arg{text} (an index
  one off the end)."
  (text :string)
  (length :int)
  (paragraph-delimiter-index :int)
  (next-paragraph-start :int))

(export 'find-paragraph-boundary)

;;; ----------------------------------------------------------------------------
;;; pango_default_break () -> default-break
;;; ----------------------------------------------------------------------------

(defcfun ("pango_default_break" %default-break) :void
  (text :string)
  (length :int)
  (analysis (:pointer (:struct analysis)))
  (attrs :pointer)
  (attrs-len :int))

(defun default-break (text analysis attrs len)
 #+liber-documentation
 "@version{#2023-1-7}
  @argument[text]{a string with the text to break, must be valid UTF-8}
  @argument[analysis]{a @symbol{pango:analysis} instance from the
    @fun{pango:itemize} function for the text}
  @argument[attrs]{logical @symbol{pango:log-attr} attributes to fill in}
  @argument[len]{an integer with the size of the array passed as @arg{attrs}}
  @begin{short}
    This is the default break algorithm.
  @end{short}
  It applies Unicode rules without language-specific tailoring, therefore the
  analyis argument is unused and can be @code{null-pointer}.

  See the @fun{pango:tailor-break} function for language-specific breaks.
  @see-symbol{pango:analysis}
  @see-symbol{pango:log-attr}
  @see-function{pango:itemize}
  @see-funcrion{pango:tailor-break}"
  (%default-break text -1 analysis attrs len))

(export 'default-break)

;;; ----------------------------------------------------------------------------
;;; pango_tailor_break ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defcfun ("pango_tailor_break" tailor-break) :void
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[text]{a string with the text to process, must be valid UTF-8}
  @argument[length]{an integer with the length in bytes of @arg{text}}
  @argument[analysis]{a @symbol{pango:analysis} instance from the function
    @fun{pango:itemize} for @arg{text}}
  @argument[offset]{an integer with the byte offset of @arg{text} from the
    beginning of the paragraph, or -1 to ignore attributes from analysis}
  @argument[log-attrs]{array with one @symbol{pango:log-attr} instance per
    character in @arg{text}, plus one extra, to be filled in}
  @argument[log-attrs-len]{an integer with the length of @arg{log-attrs} array}
  @begin{short}
    Apply language-specific tailoring to the breaks in @arg{log-attrs}, which
    are assumed to have been produced by the function
    @fun{pango:default-break}.
  @end{short}

  If @arg{offset} is not -1, it is used to apply attributes from @arg{analysis}
  that are relevant to line breaking.

  Since 1.44
  @see-symbol{pango:analysis}
  @see-symbol{pango:log-attr}
  @see-function{pango:itemize}
  @see-function{pango:default-break}"
  (text :string)
  (length :int)
  (analysis (:pointer (:struct analysis)))
  (offset :int)
  (log-attrs (:pointer (:struct log-attr)))
  (log-attrs-len :int))

#+pango-1-44
(export 'tailor-break)

;;; ----------------------------------------------------------------------------
;;; pango_shape ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_shape" shape) :void
  #+liber-documentation
  "@version{#2021-1-12}
  @argument[text]{a string with the text to process}
  @argument[lenght]{an integer with the length (in bytes) of @arg{text}}
  @argument[analysis]{a @symbol{pango:analysis} instance from the function
    @fun{pango:itemize}}
  @argument[glyphs]{a @class{pango:glyph-string} instance in which to store
    results}
  @begin{short}
    Given a segment of text and the corresponding @symbol{pango:analysis}
    instance returned from the function @fun{pango:itemize}, convert the
    characters into glyphs.
  @end{short}
  You may also pass in only a substring of the item from the function
  @fun{pango:itemize}.

  It is recommended that you use the function @fun{pango:shape-full} instead,
  since that API allows for shaping interaction happening across text item
  boundaries.

  Note that the extra attributes in the analyis that is returned from the
  function @fun{pango:itemize} have indices that are relative to the entire
  paragraph, so you need to subtract the item offset from their indices before
  calling the function @sym{pango:shape}.
  @see-symbol{pango:analysis}
  @see-class{pango:glyph-string}
  @see-function{pango:itemize}
  @see-function{pango:shape-full}"
  (text :string)
  (length :int)
  (analysis (:pointer (:struct analysis)))
  (glyphs (g:boxed glyph-string)))

(export 'shape)

;;; ----------------------------------------------------------------------------
;;; pango_shape_full ()
;;; ----------------------------------------------------------------------------

(defcfun ("pango_shape_full" shape-full) :void
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[item-text]{a string with valid UTF-8 text to shape}
  @argument[item-length]{an integer with the length (in bytes) of
    @arg{item-text}, -1 means @code{nul}-terminated text}
  @argument[paragraph-text]{a string with the text of the paragraph (see
    details), may be @code{NULL}}
  @argument[paragraph-length]{an integer with the length (in bytes) of
    @arg{paragraph-text}, -1 means @code{nul}-terminated text}
  @argument[analysis]{a @symbol{pango:analysis} instance from the function
    @fun{pango:itemize}}
  @argument[glyphs]{a @symbol{pango:glyph-string} instance in which to store
    results}
  @begin{short}
    Given a segment of text and the corresponding @symbol{pango:analysis}
    instance returned from the function @fun{pango:itemize}, convert the
    characters into glyphs.
  @end{short}
  You may also pass in only a substring of the item from the function
  @fun{pango:itemize}.

  This is similar to the function @fun{pango:shape}, except it also can
  optionally take the full paragraph text as input, which will then be used to
  perform certain cross-item shaping interactions. If you have access to the
  broader text of which @arg{item-text} is part of, provide the broader text as
  @arg{paragraph-text}. If @arg{paragraph-text} is @code{NULL}, item text is
  used instead.

  Note that the extra attributes in the analyis that is returned from the
  function @fun{pango:itemize} have indices that are relative to the entire
  paragraph, so you do not pass the full paragraph text as @arg{paragraph_text},
  you need to subtract the item offset from their indices before calling the
  function @sym{pango:shape-full}.
  @see-symbol{pango:analysis}
  @see-class{pango:glyph-string}
  @see-function{pango:itemize}
  @see-function{pango:shape}"
  (text :string)
  (length :int)
  (paragraph-text :string)
  (paragraph-length :int)
  (analysis (:pointer (:struct analysis)))
  (glyphs (g:boxed glyph-string)))

(export 'shape-full)

;;; ----------------------------------------------------------------------------
;;; pango_shape_with_flags ()
;;; ----------------------------------------------------------------------------

#+pango-1-44
(defcfun ("pango_shape_with_flags" shape-with-flags) :void
 #+liber-documentation
 "@version{#2021-1-12}
  @argument[item-text]{a string with the valid UTF-8 text to shape}
  @argument[item-length]{an integer with the length (in bytes) of
    @arg{item_text}, -1 means @code{nul}-terminated text}
  @argument[paragraph-text]{a string with the text of the paragraph (see
    details), may be @code{NULL}}
  @argument[paragraph-length]{an integer with the length (in bytes) of
    @arg{paragraph-text}, -1 means @code{nul}-terminated text}
  @argument[analysis]{a @symbol{pango:analysis} instance from the function
    @fun{pango:itemize}}
  @argument[glyphs]{a @symbol{pango:glyph-string} instance in which to store
    results}
  @argument[flags]{a @symbol{pango:shape-flags} value influencing the shaping
    process}
  @begin{short}
    Given a segment of text and the corresponding @symbol{pango:analysis}
    instance returned from the function @fun{pango:itemize}, convert the
    characters into glyphs.
  @end{short}
  You may also pass in only a substring of the item from the function
  @fun{pango:itemize}.

  This is similar to the function @fun{pango:shape-full}, except it also takes
  flags that can influence the shaping process.

  Note that the extra attributes in the analyis that is returned from the
  function @fun{pango:itemize} have indices that are relative to the entire
  paragraph, so you do not pass the full paragraph text as @arg{paragraph_text},
  you need to subtract the item offset from their indices before calling the
  function @sym{pango:shape-with-flags}.

  Since 1.44
  @see-symbol{pango:analysis}
  @see-class{pango:glyph-string}
  @see-symbol{pango:shape-flags}
  @see-function{pango:itemize}
  @see-function{pango:shape-full}"
  (item-text :string)
  (item-length :int)
  (paragraph-text :string)
  (paragraph-length :int)
  (analysis (:pointer (:struct analysis)))
  (glyphs (g:boxed glyph-string))
  (flags shape-flags))

#+pango-1-44
(export 'shape-with-flags)

;;; --- End of file pango.rendering.lisp ---------------------------------------
