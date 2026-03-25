;;; ----------------------------------------------------------------------------
;;; pango.item.lisp
;;;
;;; The documentation in this file is taken from the Pango Reference Manual
;;; version 1.56 and modified to document the Lisp binding to the Pango
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2020 - 2026 Dieter Kaiser
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
;;; Rendering
;;;
;;;     Functions to run the rendering pipeline
;;;
;;; Types and Values
;;;
;;;     PangoShapeFlags
;;;     PangoLogAttr                                        not exported
;;;     PangoAnalysis
;;;     PangoItem
;;;
;;; Functions
;;;
;;;     pango_item_new
;;;     pango_item_copy
;;;     pango_item_free                                     not needed
;;;     pango_item_split
;;;     pango_item_apply_attrs
;;;     pango_item_get_char_offset                          Since 1.54
;;;
;;;     pango_itemize
;;;     pango_itemize_with_base_dir                         not needed
;;;     pango_reorder_items                                 not implemented
;;;     pango_break                                         not implemented
;;;     pango_get_log_attrs                                 not implemented
;;;     pango_find_paragraph_boundary                       not implemented
;;;     pango_default_break                                 not exported
;;;     pango_tailor_break                                  not exported
;;;     pango_shape
;;;     pango_shape_full                                    not implemented
;;;     pango_shape_with_flags                              not needed
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── PangoItem
;;;
;;;     GFlags
;;;     ╰── PangoShapeFlags
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoShapeFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "PangoShapeFlags" shape-flags
  (:export t
   :type-initializer "pango_shape_flags_get_type")
  (:none 0)
  (:round-positions #.(ash 1 0)))

#+liber-documentation
(setf (liber:alias-for-symbol 'shape-flags)
      "GFlags"
      (liber:symbol-documentation 'shape-flags)
 "@version{2026-03-11}
  @begin{declaration}
(gobject:define-gflags \"PangoShapeFlags\" shape-flags
  (:export t
   :type-initializer \"pango_shape_flags_get_type\")
  (:none 0)
  (:round-positions #.(ash 1 0)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{Default value.}
      @entry[:round-positions]{Round glyph positions and widths to whole device
        units. This option should be set if the target renderer cannot do
        subpixel positioning of glyphs.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Flags influencing the shaping process.
  @end{short}
  These can be passed to the @fun{pango:shape} function.
  @see-function{pango:shape}")

;;; ----------------------------------------------------------------------------
;;; PangoLogAttr                                            not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation

(cffi:defcstruct log-attr
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
 "@version{2025-08-24}
  @begin{declaration}
(cffi:defcstruct log-attr
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
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[is-line-break]{If set, can break line in front of character.}
      @entry[is-mandatory-break]{If set, must break line in front of character.}
      @entry[is-char-break]{If set, can break here when doing character
        wrapping.}
      @entry[is-white]{Is whitespace character.}
      @entry[is-cursor-position]{If set, cursor can appear in front of
        character. That is, this is a grapheme boundary, or the first character
        in the text. This flag implements Unicode's Grapheme Cluster Boundaries
        semantics.}
      @entry[is-word-start]{Is first character in a word.}
      @entry[is-word-end]{Is first non-word char after a word. Note that in
        degenerate cases, you could have both the  @code{is-word-start} value
        and the @code{is-word-end} value set for some character.}
      @entry[is-sentence-boundary]{Is a sentence boundary. There are two ways
        to divide sentences. The first assigns all inter-sentence
        whitespace/control/format chars to some sentence, so all chars are in
        some sentence. The @code{is-sentence-boundary} value denotes the
        boundaries there. The second way does not assign between-sentence
        spaces, and so on. to any sentence, so the @code{is-sentence_start} /
        @code{is-sentence-end} values mark the boundaries of those sentences.}
      @entry[is-sentence-start]{Is first character in a sentence.}
      @entry[is-sentence-end]{Is first char after a sentence. Note that in
        degenerate cases, you could have both the @code{is-sentence-start} value
        and the @code{is-sentence-end} value set for some character, for example
        no space after a period, so the next sentence starts right away.}
      @entry[backspace-deletes-character]{If set, backspace deletes one
        character rather than the entire grapheme cluster. This field is only
        meaningful on grapheme boundaries, where the @code{is-cursor-position}
        value is set. In some languages, the full grapheme, for example
        @code{letter + diacritics}, is considered a unit, while in others, each
        decomposed character in the grapheme is a unit.}
      @entry[is-expandable-space]{Is a whitespace character that can possibly
        be expanded for justification purposes.}
      @entry[is-word-boundary]{Is a word boundary. More specifically, means
        that this is not a position in the middle of a word. For example, both
        sides of a punctuation mark are considered word boundaries. This flag
        is particularly useful when selecting text word-by-word. This flag
        implements Unicode's Word Boundaries semantics.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{pango:log-attr} structure stores information about the attributes
    of a single character.
  @end{short}
  @see-class{pango:layout}")

;;; ----------------------------------------------------------------------------
;;; PangoAnalysis
;;; ----------------------------------------------------------------------------

(cffi:defcstruct analysis
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
 "@version{2026-03-11}
  @begin{declaration}
(cffi:defcstruct analysis
  (shape-engine :pointer)
  (lang-engine :pointer)
  (font (g:object font))
  (level :uint8)
  (gravity :uint8)
  (flags :uint8)
  (script :uint8)
  (language (g:boxed language))
  (extra-attrs :pointer))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[shape-engine]{Unused.}
      @entry[lang-engine]{Unused.}
      @entry[font]{The @class{pango:font} object for the font of this segment.}
      @entry[level]{The bidirectional level for this segment.}
      @entry[gravity]{The @sym{pango:gravity} value for the glyph orientation
        of this segment.}
      @entry[flags]{Boolean flags for this segment.}
      @entry[script]{The @sym{pango:script} value for the detected script of
        this segment.}
      @entry[language]{The @class{pango:language} object for the detected
        language of this segment.}
      @entry[extra-attrs]{Extra attributes for this segment.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{pango:analysis} structure stores information about the properties
    of a segment of text.
  @end{short}
  The @class{pango:item} structure contains a field for a
  @symbol{pango:analysis} instance. After text has been itemised using the
  @fun{pango:itemize} function, the @symbol{pango:analysis} instance is
  populated with values for each @class{pango:item} instance resulting from the
  itemisation process. This instance is then passed to the @fun{pango:shape}
  function, which converts the text into glyphs. See the @fun{pango:shape}
  documentation for a complete example.
  @see-slot{pango:analysis-font}
  @see-slot{pango:analysis-level}
  @see-slot{pango:analysis-gravity}
  @see-slot{pango:analysis-flags}
  @see-slot{pango:analysis-script}
  @see-slot{pango:analysis-language}
  @see-slot{pango:analysis-extra-attrs}
  @see-class{pango:item}
  @see-class{pango:font}
  @see-symbol{pango:gravity}
  @see-symbol{pango:script}
  @see-class{pango:language}
  @see-function{pango:itemize}
  @see-function{pango:shape}")

(export 'analysis)

;;; --- pango:analysis-font ----------------------------------------------------

(defun analysis-font (instance)
  (cffi:foreign-slot-value instance '(:struct analysis) 'font))

#+liber-documentation
(setf (liber:alias-for-function 'analysis-font)
      "Accessor"
      (documentation 'analysis-font 'function)
 "@version{2026-03-11}
  @syntax{(pango:analysis-font instance) => font}
  @argument[instance]{a @sym{pango:analysis} instance}
  @argument[font]{a @class{pango:font} instance}
  @begin{short}
    The accessor for the @code{font} slot of the @sym{pango:analysis} structure.
  @end{short}
  @see-symbol{pango:analysis}
  @see-class{pango:font}")

(export 'analysis-font)

;;; --- pango:analysis-level ---------------------------------------------------

(defun analysis-level (instance)
  (cffi:foreign-slot-value instance '(:struct analysis) 'level))

#+liber-documentation
(setf (liber:alias-for-function 'analysis-level)
      "Accessor"
      (documentation 'analysis-level 'function)
 "@version{2026-03-11}
  @syntax{(pango:analysis-level instance) => level}
  @argument[instance]{a @sym{pango:analysis} instance}
  @argument[font]{an unsigned integer for the bidirectional level of this
    segment}
  @begin{short}
    The accessor for the @code{level} slot of the @sym{pango:analysis}
    structure.
  @end{short}
  @see-symbol{pango:analysis}")

(export 'analysis-level)

;;; --- pango:analysis-gravity -------------------------------------------------

(defun analysis-gravity (instance)
  (cffi:convert-from-foreign
      (cffi:foreign-slot-value instance '(:struct analysis) 'gravity)
      'gravity))

#+liber-documentation
(setf (liber:alias-for-function 'analysis-gravity)
      "Accessor"
      (documentation 'analysis-gravity 'function)
 "@version{2026-03-11}
  @syntax{(pango:analysis-gravity instance) => gravity}
  @argument[instance]{a @sym{pango:analysis} instance}
  @argument[gravity]{a @sym{pango:gravity} value for the glyph orientation}
  @begin{short}
    The accessor for the @code{gravity} slot of the @sym{pango:analysis}
    structure.
  @end{short}
  @see-symbol{pango:analysis}
  @see-symbol{pango:gravity}")

(export 'analysis-gravity)

;;; --- pango:analysis-flags ---------------------------------------------------

;; TODO: More documentation for the returned flags is needed. Do we have a
;; type for these flags!?

(defun analysis-flags (instance)
  (cffi:foreign-slot-value instance '(:struct analysis) 'flags))

#+liber-documentation
(setf (liber:alias-for-function 'analysis-flags)
      "Accessor"
      (documentation 'analysis-flags 'function)
 "@version{2026-03-11}
  @syntax{(pango:analysis-flags instance) => flags}
  @argument[instance]{a @sym{pango:analysis} instance}
  @argument[flags]{an unsigned integer for the boolean flags of this segment}
  @begin{short}
    The accessor for the @code{flags} slot of the @sym{pango:analysis}
    structure.
  @end{short}
  @see-symbol{pango:analysis}")

(export 'analysis-flags)

;;; --- pango:analysis-script --------------------------------------------------

(defun analysis-script (instance)
  (cffi:convert-from-foreign
      (cffi:foreign-slot-value instance '(:struct analysis) 'script)
      'script))

#+liber-documentation
(setf (liber:alias-for-function 'analysis-script)
      "Accessor"
      (documentation 'analysis-script 'function)
 "@version{2026-03-11}
  @syntax{(pango:analysis-script instance) => script}
  @argument[instance]{a @sym{pango:analysis} instance}
  @argument[script]{a @sym{pango:script} value for the detected script}
  @begin{short}
    The accessor for the @code{script} slot of the @sym{pango:analysis}
    structure.
  @end{short}
  @see-symbol{pango:analysis}
  @see-symbol{pango:script}")

(export 'analysis-script)

;;; --- pango:analysis-language ------------------------------------------------

(defun analysis-language (instance)
  (cffi:foreign-slot-value instance '(:struct analysis) 'language))

#+liber-documentation
(setf (liber:alias-for-function 'analysis-language)
      "Accessor"
      (documentation 'analysis-language 'function)
 "@version{2026-03-11}
  @syntax{(pango:analysis-language instance) => language}
  @argument[instance]{a @sym{pango:analysis} instance}
  @argument[language]{a @class{pango:language} instance for the detected
    language}
  @begin{short}
    The accessor for the @code{language} slot of the @sym{pango:analysis}
    structure.
  @end{short}
  @see-symbol{pango:analysis}
  @see-class{pango:language}")

(export 'analysis-language)

;;; --- pango:analysis-extra-attrs ---------------------------------------------

(defun analysis-extra-attrs (instance)
  (let ((ptr (cffi:foreign-slot-value instance
                                      '(:struct analysis) 'extra-attrs)))
    (unless (cffi:null-pointer-p ptr)
      (cffi:convert-from-foreign ptr
                                 '(glib:slist-t (g:boxed attribute :return))))))

#+liber-documentation
(setf (liber:alias-for-function 'analysis-extra-attrs)
      "Accessor"
      (documentation 'analysis-extra-attrs 'function)
 "@version{2026-03-11}
  @syntax{(pango:analysis-extra-attrs instance) => attrs}
  @argument[instance]{a @sym{pango:analysis} instance}
  @argument[attrs]{a list of @class{pango:attribute} instances for extra
    attributes}
  @begin{short}
    The accessor for the @code{extra-attrs} slot of the @sym{pango:analysis}
    structure.
  @end{short}
  @see-symbol{pango:analysis}
  @see-class{pango:attribute}")

(export 'analysis-extra-attrs)

;;; ----------------------------------------------------------------------------
;;; PangoItem
;;; ----------------------------------------------------------------------------

;; This declaration is necessary to allow the usage of pango:analysis in a
;; nested defcstruct for pango:item.
(cffi:defctype analysis (:struct analysis))

;; PangoItem is implemented as an opaque GBoxed type. The CStruct GBoxed type
;; cannot handle the nested CStruct correctly.

(cffi:defcstruct %item
  (offset :int)
  (length :int)
  (num-chars :int)
  (analysis analysis))

(cffi:defcfun ("pango_item_new" %item-new) :pointer)

(glib:define-gboxed-opaque item "PangoItem"
   :export t
   :type-initializer "pango_item_get_type"
   :alloc (%item-new))

#+liber-documentation
(setf (liber:alias-for-class 'item)
      "GBoxed"
      (documentation 'item 'type)
 "@version{2026-03-11}
  @begin{declaration}
(glib:define-gboxed-opaque item \"PangoItem\"
   :export t
   :type-initializer \"pango_item_get_type\"
   :alloc (%item-new))
  @end{declaration}
  @begin{short}
    The @class{pango:item} structure stores information about a segment of text.
  @end{short}
  The @fun{pango:itemize} function breaks up a piece of text into
  @class{pango:item} instances, each of which represents a segment of text.

  The @class{pango:item} instance stores the byte offset of the start of the
  item in text, the length of the item in bytes, the number of characters in
  the item and the @symbol{pango:analysis} instance for the analysis results of
  the item. The values can be accessed with the corresponding functions:
  @fun{pango:item-offset}, @fun{pango:item-length}, @fun{pango:item-num-chars}
  and @fun{pango:item-analysis}.
  @see-constructor{pango:item-new}
  @see-constructor{pango:item-copy}
  @see-function{pango:item-analysis}
  @see-function{pango:item-length}
  @see-function{pango:item-num-chars}
  @see-function{pango:item-offset}
  @see-symbol{pango:analysis}
  @see-function{pango:itemize}")

;;; --- pango:item-analysis ----------------------------------------------------

(defun item-analysis (instance)
  (let ((ptr (glib:boxed-opaque-pointer instance)))
    (cffi:foreign-slot-value ptr '(:struct %item) 'analysis)))

#+liber-documentation
(setf (documentation 'item-analysis 'function)
 "@version{2026-03-11}
  @syntax{(pango:item-analysis instance) => analysis}
  @argument[instance]{a @class{pango:item} instance}
  @argument[analysis]{a @sym{pango:analysis} instance}
  @begin{short}
    Gets the @symbol{pango:analysis} instance for the analysis results of the
    Pango item.
  @end{short}
  @see-class{pango:item}
  @see-symbol{pango:analysis}")

(export 'item-analysis)

;;; --- pango:item-length ------------------------------------------------------

(defun item-length (instance)
  (let ((ptr (glib:boxed-opaque-pointer instance)))
    (cffi:foreign-slot-value ptr '(:struct %item) 'length)))

#+liber-documentation
(setf (documentation 'item-length 'function)
 "@version{2026-03-11}
  @syntax{(pango:item-length instance) => length}
  @argument[instance]{a @class{pango:item} instance}
  @argument[length]{an integer for the length of the item in bytes}
  @begin{short}
    Gets the length of the Pango item in bytes.
  @end{short}
  @see-class{pango:item}")

(export 'item-length)

;;; --- pango:item-num-chars ---------------------------------------------------

(defun item-num-chars (instance)
  (let ((ptr (glib:boxed-opaque-pointer instance)))
    (cffi:foreign-slot-value ptr '(:struct %item) 'num-chars)))

#+liber-documentation
(setf (documentation 'item-num-chars 'function)
 "@version{2026-03-11}
  @syntax{(pango:item-num-chars instance) => num}
  @argument[instance]{a @class{pango:item} instance}
  @argument[num]{an integer for the number of Unicode characters in the item}
  @begin{short}
    Gets the number of Unicode characters in the Pango item.
  @end{short}
  @see-class{pango:item}")

(export 'item-num-chars)

;;; --- pango:item-offset ------------------------------------------------------

(defun item-offset (instance)
  (let ((ptr (glib:boxed-opaque-pointer instance)))
    (cffi:foreign-slot-value ptr '(:struct %item) 'offset)))

#+liber-documentation
(setf (documentation 'item-offset 'function)
 "@version{2026-03-11}
  @syntax{(pango:item-offset instance) => offset}
  @argument[instance]{a @class{pango:item} instance}
  @argument[offset]{an integer for the byte offset of the start of this item
    in the text}
  @begin{short}
    Gets the byte offset of the start of this item in text.
  @end{short}
  @see-class{pango:item}")

(export 'item-offset)

;;; ----------------------------------------------------------------------------
;;; pango_item_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_item_new" item-new) (g:boxed item :return)
 #+liber-documentation
 "@version{2026-03-11}
  @return{The newly allocated @class{pango:item} instance.}
  @begin{short}
    Creates a new @class{pango:item} instance initialized to default values.
  @end{short}
  @see-class{pango:item}
  @see-function{pango:item-copy}")

(export 'item-new)

;;; ----------------------------------------------------------------------------
;;; pango_item_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_item_copy" item-copy) (g:boxed item :return)
 #+liber-documentation
 "@version{2026-03-11}
  @argument[item]{a @class{pango:item} instance}
  @return{The newly allocated @class{pango:item} instance.}
  @begin{short}
    Copy an existing @class{pango:item} instance.
  @end{short}
  @see-class{pango:item}
  @see-function{pango:item-new}"
  (item (g:boxed item)))

(export 'item-copy)

;;; ----------------------------------------------------------------------------
;;; pango_item_free                                         not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_item_split
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_item_split" item-split) (g:boxed item :return)
 #+liber-documentation
 "@version{2026-03-11}
  @argument[item]{a @class{pango:item} instance}
  @argument[index]{an integer for the byte index of the position to split
    @arg{item}, relative to the start of the item}
  @argument[offset]{an integer for the number of chars between the start of
    @arg{item} and @arg{index}}
  @begin{return}
    The new @class{pango:item} instance representing text before @arg{index}.
  @end{return}
  @begin{short}
    Modifies @arg{item} to cover only the text after @arg{index}, and returns a
    new item that covers the text before @arg{index} that used to be in
    @arg{item}.
  @end{short}
  You can think of @arg{index} as the length of the returned item.

  The @arg{index} argument may not be 0, and it may not be greater than or equal
  to the length of @arg{item}, that is, there must be at least one byte assigned
  to each item, you cannot create a zero-length item.

  The @arg{offset} argument is the length of the first item in chars, and must
  be provided because the text used to generate the item is not available, so
  the @sym{pango:item-split} function cannot count the char length of the split
  items itself.
  @see-class{pango:item}"
  (item (g:boxed item))
  (index :int)
  (offset :int))

(export 'item-split)

;;; ----------------------------------------------------------------------------
;;; pango_item_apply_attrs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_item_apply_attrs" item-apply-attrs) :void
 #+liber-documentation
 "@version{2026-03-11}
  @argument[item]{a @class{pango:item} instance}
  @argument[iter]{a @class{pango:attr-iterator} instance}
  @begin{short}
    Add attributes to a @class{pango:item} instance.
  @end{short}
  The idea is that you have attributes that do not affect itemization, such as
  font features, so you filter them out using the @fun{pango:attr-list-filter}
  function, itemize your text, then reapply the attributes to the resulting
  items using this function.

  The @arg{iter} iterator should be positioned before the range of the item,
  and will be advanced past it. This function is meant to be called in a loop
  over the items resulting from itemization, while passing the iterator to each
  call.
  @see-class{pango:item}
  @see-class{pango:attr-iterator}
  @see-function{pango:attr-list-filter}"
  (item (g:boxed item))
  (iter (g:boxed attr-iterator)))

(export 'item-apply-attrs)

;;; ----------------------------------------------------------------------------
;;; pango_item_get_char_offset
;;; ----------------------------------------------------------------------------

#+pango-1-54
(cffi:defcfun ("pango_item_get_char_offset" item-char-offset) :int
 #+liber-documentation
 "@version{2026-03-11}
  @argument[item]{a @class{pango:item} instance}
  @return{The integer for the character offset, or -1.}
  @begin{short}
    Returns the character offset of the item from the beginning of the itemized
    text.
  @end{short}
  If the item has not been obtained from Pango’s itemization machinery, then
  the character offset is not available. In that case, this function returns -1.

  Since 1.54
  @see-class{pango:item}"
  (item (g:boxed item)))

#+pango-1-54
(export 'item-char-offset)

;;; ----------------------------------------------------------------------------
;;; pango_itemize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_itemize_with_base_dir" %itemize-with-base-dir)
    (g:list-t (g:boxed item :return))
  (context (g:object context))
  (direction direction)
  (text :string)
  (start :int)
  (len :int)
  (attrs (g:boxed attr-list))
  (iter (g:boxed attr-iterator)))

(defun itemize (context text start len attrs iter &key direction)
 #+liber-documentation
 "@version{2026-03-23}
  @argument[context]{a @class{pango:context} object holding information that
    affects the itemization process}
  @argument[text]{a string for the text to itemize, must be valid UTF-8}
  @argument[start]{an integer for the first byte in @arg{text} to process}
  @argument[len]{an integer for the number of bytes, (not characters, to
    process after @arg{start}, this must be >= 0}
  @argument[attrs]{a @class{pango:attr-list} instance for the set of attributes
    that apply to @arg{text}}
  @argument[iter]{a cached @class{pango:attr-iterator} attribute iterator, or
    @code{nil}}
  @argument[direction]{a @sym{pango:direction} value for the base direction to
    use for bidirectional processing, or the default @code{nil} value}
  @return{The list of @class{pango:item} instances.}
  @begin{short}
    Breaks a piece of text into segments with consistent directional level and
    shaping engine.
  @end{short}
  Each byte of @arg{text} will be contained in exactly one of the items in the
  returned list. The generated list of items will be in logical order, the
  start offsets of the items are ascending.

  The @arg{iter} argument should be an iterator over @arg{attrs} currently
  positioned at a range before or containing @arg{start}. The @arg{iter}
  argument will be advanced to the range covering the position just after
  @arg{start} + @arg{len}, that is, if itemizing in a loop, just keep passing
  in the same @arg{iter}.

  The @arg{direction} argument specifies the base direction to use when
  computing bidirectional levels. For the default @code{nil} value the base
  direction is obtained from the @class{pango:context} object using the
  @fun{pango:context-base-dir} function.
  @begin[Notes]{dictionary}
    This function includes the functionality of the
    @code{pango_itemize_with_base_dir()} function using the @arg{direction}
    keyword argument.
  @end{dictionary}
  @see-class{pango:context}
  @see-class{pango:item}
  @see-class{pango:attr-list}
  @see-class{pango:attr-iterator}
  @see-symbol{pango:direction}
  @see-function{pango:context-base-dir}"
  (let ((direction (or direction
                       (context-base-dir context))))
    (%itemize-with-base-dir context direction text start len attrs iter)))

(export 'itemize)

;;; ----------------------------------------------------------------------------
;;; pango_itemize_with_base_dir                             not needed
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_itemize_with_base_dir" itemize-with-base-dir)
    (g:list-t (g:boxed item :return))
 #+liber-documentation
 "@version{2025-08-24}
  @argument[context]{a @class{pango:context} object holding information that
    affects the itemization process}
  @argument[direction]{a @sym{pango:direction} value for the base direction to
    use for bidirectional processing}
  @argument[text]{a string for the text to itemize, must be valid UTF-8}
  @argument[start]{an integer for the first byte in @arg{text} to process}
  @argument[length]{an integer for the number of bytes, not characters, to
    process after @arg{start}, this must be >= 0}
  @argument[attrs]{a @class{pango:attr-list} instance for the set of
    attributes that apply to @arg{text}}
  @argument[iter]{a cached @class{pango:attr-iterator} attribute iterator, or
    @code{nil}}
  @return{The list of @class{pango:item} instances.}
  @begin{short}
    Like the @fun{pango:itemize} function, but the base direction to use when
    computing bidirectional levels, is specified explicitly rather than gotten
    from the @class{pango:context} object.
  @end{short}
  See also the @fun{pango:context-base-dir} function.
  @see-class{pango:context}
  @see-class{pango:item}
  @see-class{pango:attr-list}
  @see-class{pango:attr-iterator}
  @see-symbol{pango:direction}
  @see-function{pango:itemize}
  @see-function{pango:context-base-dir}"
  (context (g:object context))
  (direction direction)
  (text :string)
  (start :int)
  (length :int)
  (attrs (g:boxed attr-list))
  (iter (g:boxed attr-iterator)))

#+nil
(export 'itemize-with-base-dir)

;;; ----------------------------------------------------------------------------
;;; pango_reorder_items
;;;
;;; Reorder items from logical order to visual order.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_break
;;;
;;; Determines possible line, word, and character breaks for a string of
;;; Unicode text with a single analysis.
;;;
;;; Deprecated 1.44
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_get_log_attrs
;;;
;;; Computes a PangoLogAttr for each character in text.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_find_paragraph_boundary
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_find_paragraph_boundary" %find-paragraph-boundary) :void
  (text :string)
  (len :int)
  (index (:pointer :int))
  (next (:pointer :int)))

(defun find-paragraph-boundary (text)
 #+liber-documentation
 "@version{2026-03-17}
  @syntax{(pango:find-paragraph-boundary text) => index, next}
  @argument[text]{a string for the UTF-8 text}
  @argument[index]{an integer for the index of delimiter}
  @argument[next]{an integer for the start of next paragraph}
  @begin{short}
    Locates a paragraph boundary in @arg{text}.
  @end{short}
  A boundary is caused by delimiter characters, such as a newline, carriage
  return, carriage return-newline pair, or Unicode paragraph separator
  character.

  The index of the run of delimiters is returned in @arg{index}. The index of
  the start of the next paragraph (index after all delimiters) is stored in
  @arg{next}.

  If no delimiters are found, both @arg{index} and @arg{next} are filled with
  the length of text (an index one off the end).
  @see-class{pango:item}
  @see-function{pango:itemize}
  @see-function{pango:shape}"
  (cffi:with-foreign-objects((index :int) (next :int))
    (%find-paragraph-boundary text -1 index next)
    (values (cffi:mem-ref index :int)
            (cffi:mem-ref next :int))))

(export 'find-paragraph-boundary)

;;; ----------------------------------------------------------------------------
;;; pango_default_break                                     not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation

(cffi:defcfun ("pango_default_break" %default-break) :void
  (text :string)
  (length :int)
  (analysis (:pointer (:struct analysis)))
  (attrs :pointer)
  (attrs-len :int))

(defun default-break (text analysis attrs len)
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[text]{a string for the text to break, must be valid UTF-8}
  @argument[analysis]{a @sym{pango:analysis} instance from the
    @fun{pango:itemize} function for the text}
  @argument[attrs]{logical @sym{pango:log-attr} attributes to fill in}
  @argument[len]{an integer for the size of the array passed as @arg{attrs}}
  @begin{short}
    This is the default break algorithm.
  @end{short}
  It applies Unicode rules without language-specific tailoring, therefore the
  analyis argument is unused and can be @code{cffi:null-pointer}.

  See the @fun{pango:tailor-break} function for language-specific breaks.
  @see-symbol{pango:analysis}
  @see-symbol{pango:log-attr}
  @see-function{pango:itemize}
  @see-funcrion{pango:tailor-break}"
  (%default-break text -1 analysis attrs len))

;;; ----------------------------------------------------------------------------
;;; pango_tailor_break                                      not exported
;;; ----------------------------------------------------------------------------

;; TODO: Consider to remove the implementation

(cffi:defcfun ("pango_tailor_break" tailor-break) :void
 #+liber-documentation
 "@version{#2025-08-24}
  @argument[text]{a string for the text to process, must be valid UTF-8}
  @argument[length]{an integer for the length in bytes of @arg{text}}
  @argument[analysis]{a @sym{pango:analysis} instance from the function
    @fun{pango:itemize} for @arg{text}}
  @argument[offset]{an integer for the byte offset of @arg{text} from the
    beginning of the paragraph, or -1 to ignore attributes from analysis}
  @argument[log-attrs]{array with one @sym{pango:log-attr} instance per
    character in @arg{text}, plus one extra, to be filled in}
  @argument[log-attrs-len]{an integer for the length of @arg{log-attrs} array}
  @begin{short}
    Apply language-specific tailoring to the breaks in @arg{log-attrs}, which
    are assumed to have been produced by the function
    @fun{pango:default-break}.
  @end{short}

  If @arg{offset} is not -1, it is used to apply attributes from @arg{analysis}
  that are relevant to line breaking.
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

;;; ----------------------------------------------------------------------------
;;; pango_shape
;;; ----------------------------------------------------------------------------

#+nil
(cffi:defcfun ("pango_shape" %shape) :void
  (text :string)
  (len :int)
  (analysis (:pointer (:struct analysis)))
  (glyphs (g:boxed glyph-string)))

;; This is the most general implementation
(cffi:defcfun ("pango_shape_with_flags" %shape-with-flags) :void
  (item-text :string)
  (item-length :int)
  (paragraph-text :string)
  (paragraph-length :int)
  (analysis (:pointer (:struct analysis)))
  (glyphs (g:boxed glyph-string))
  (flags shape-flags))

(defun shape (text analysis &optional (flags :none))
 #+liber-documentation
 "@version{2026-03-11}
  @argument[text]{a string for the text to process}
  @argument[analysis]{a @sym{pango:analysis} instance from the
    @fun{pango:itemize} function}
  @argument[flags]{an optional @symbol{pango:shape-flags} value, the default is
    @val[pango:shape-flags]{:none}}
  @return[glyphs]{The @class{pango:glyph-string} instance for the results}
  @begin{short}
    Given a segment of text and the corresponding @sym{pango:analysis} instance
    returned from the @fun{pango:itemize} function, convert the characters into
    glyphs.
  @end{short}
  You may also pass in only a substring of the item from the @fun{pango:itemize}
  function. It also takes @arg{flags} that can influence the shaping process.

  Note that the extra attributes in the analyis that is returned from the
  @fun{pango:itemize} function have indices that are relative to the entire
  paragraph, so you need to subtract the item offset from their indices before
  calling the @fun{pango:shape} function.
  @see-symbol{pango:analysis}
  @see-class{pango:glyph-string}
  @see-symbol{pango:shape-flags}
  @see-function{pango:itemize}"
  (let ((glyphs (glyph-string-new)))
    (%shape-with-flags text -1 (cffi:null-pointer) -1 analysis glyphs flags)
    glyphs))

(export 'shape)

;;; ----------------------------------------------------------------------------
;;; pango_shape_full
;;; ----------------------------------------------------------------------------

;; TODO: This implementation does not work. TEXT must be a pointer into
;; PARAGRAPH for this to work.

#+nil
(cffi:defcfun ("pango_shape_full" %shape-full) :void
  (text :string)
  (len :int)
  (paragraph-text :string)
  (paragraph-len :int)
  (analysis (:pointer (:struct analysis)))
  (glyphs (g:boxed glyph-string)))

#+nil
(defun shape-full (text paragraph analysis)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[text]{a string for the valid UTF-8 text to shape}
  @argument[paragraph]{a string for the text of the paragraph, may be
    @code{nil}}
  @argument[analysis]{a @sym{pango:analysis} instance from the
    @fun{pango:itemize} function}
  @return{The @sym{pango:glyph-string} instance for the result.}
  @begin{short}
    Given a segment of text and the corresponding @sym{pango:analysis} instance
    returned from the @fun{pango:itemize} function, convert the characters into
    glyphs.
  @end{short}
  You may also pass in only a substring of the item from the @fun{pango:itemize}
  function.

  This is similar to the @fun{pango:shape} function, except it also can
  optionally take the full paragraph text as input, which will then be used to
  perform certain cross-item shaping interactions. If you have access to the
  broader text of which @arg{text} is part of, provide the broader text as
  @arg{paragraph}. If @arg{paragraph} is @code{nil}, item text is used instead.

  Note that the extra attributes in the analyis that is returned from the
  @fun{pango:itemize} function have indices that are relative to the entire
  paragraph, so you do not pass the full paragraph text as @arg{paragraph}, you
  need to subtract the item offset from their indices before calling the
  @fun{pango:shape-full} function.
  @see-symbol{pango:analysis}
  @see-class{pango:glyph-string}
  @see-function{pango:itemize}
  @see-function{pango:shape}"
  (let ((glyphs (glyph-string-new))
        (paragraph (or paragraph (cffi:null-pointer))))
    (%shape-full text -1 paragraph -1 analysis glyphs)
    glyphs))

#+nil
(export 'shape-full)

;;; ----------------------------------------------------------------------------
;;; pango_shape_with_flags                                  not needed
;;; ----------------------------------------------------------------------------

;; TODO: This implementation does not work. TEXT must be a pointer into
;; PARAGRAPH for this to work. The FLAGS argument is provided as an optional
;; argument of the PANGO:SHAPE function.

#+nil
(cffi:defcfun ("pango_shape_with_flags" %shape-with-flags) :void
  (item-text :string)
  (item-length :int)
  (paragraph-text :string)
  (paragraph-length :int)
  (analysis (:pointer (:struct analysis)))
  (glyphs (g:boxed glyph-string))
  (flags shape-flags))

#+nil
(defun shape-with-flags (text paragraph analysis flags)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[text]{a string for the valid UTF-8 text to shape}
  @argument[paragraph]{a string for the text of the paragraph, may be
    @code{nil}}
  @argument[analysis]{a @sym{pango:analysis} instance from the
    @fun{pango:itemize} function}
  @argument[flags]{a @sym{pango:shape-flags} value influencing the shaping
    process}
  @return{The @sym{pango:glyph-string} instance for the result.}
  @begin{short}
    Given a segment of text and the corresponding @sym{pango:analysis} instance
    returned from the @fun{pango:itemize} function, convert the characters into
    glyphs.
  @end{short}
  You may also pass in only a substring of the item from the @fun{pango:itemize}
  function.

  This is similar to the @fun{pango:shape-full} function, except it also takes
  flags that can influence the shaping process.

  Note that the extra attributes in the analyis that is returned from the
  @fun{pango:itemize} function have indices that are relative to the entire
  paragraph, so you do not pass the full paragraph text as @arg{paragraph}, you
  need to subtract the item offset from their indices before calling the
  @fun{pango:shape-with-flags} function.
  @see-symbol{pango:analysis}
  @see-class{pango:glyph-string}
  @see-symbol{pango:shape-flags}
  @see-function{pango:itemize}
  @see-function{pango:shape-full}"
  (let ((glyphs (glyph-string-new))
        (paragraph (or paragraph (cffi:null-pointer))))
    (%shape-with-flags text -1 paragraph -1 analysis glyphs flags)
    glyphs))

#+nil
(export 'shape-with-flags)

;;; --- End of file pango.item.lisp --------------------------------------------
