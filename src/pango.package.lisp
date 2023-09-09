;;; ----------------------------------------------------------------------------
;;; pango.package.lisp
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defpackage :pango
  (:use :common-lisp)
  (:import-from :cffi)
  (:import-from #:glib)
  (:import-from #:gobject))

(in-package :pango)

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (find-package :pango) t)
 "Pango is a text layout and shaping library. Pango facilitates the
  layout and shaping of multi-language text. Full-function rendering of text
  and cross-platform support is had when Pango is used with platform APIs or
  3rd party libraries, such as Uniscribe and FreeType, as text rendering
  backends. Pango-processed text will appear similar under different operating
  systems. This is the API documentation of a Lisp binding to Pango.
  @begin[Basic Pango Interfaces]{section}
    @begin[Rendering]{subsection}
      The Pango rendering pipeline takes a string of Unicode characters and
      converts it into glyphs. The functions described in this section
      accomplish various steps of this process.
      @about-symbol{PANGO_ANALYSIS_FLAG_CENTERED_BASELINE}
      @about-symbol{PANGO_ANALYSIS_FLAG_IS_ELLIPSIS}
      @about-symbol{PANGO_ANALYSIS_FLAG_NEED_HYPHEN}
      @about-symbol{shape-flags}
      @about-symbol{log-attr}
      @about-symbol{analysis}
      @about-struct{item}
      @about-function{item-analysis}
      @about-function{item-length}
      @about-function{item-num-chars}
      @about-function{item-offset}
      @about-function{item-new}
      @about-function{item-copy}
      @about-function{item-split}
      @about-function{item-apply-attrs}
      @about-function{itemize}
      @about-function{itemize-with-base-dir}
      @about-function{reorder-items}
      @about-function{log-attrs}
      @about-function{find-paragraph-boundary}
      @about-function{default-break}
      @about-function{tailor-break}
      @about-function{shape}
      @about-function{shape-full}
      @about-function{shape-with-flags}
    @end{subsection}
    @begin[Fonts]{subsection}
      Structures representing abstract fonts. Pango supports a flexible
      architecture where a particular rendering architecture can supply an
      implementation of fonts. The @class{pango:font} class represents an
      abstract rendering-system-independent font. Pango provides routines to
      list available fonts, and to load a font of a given description.
      @about-symbol{style}
      @about-symbol{weight}
      @about-symbol{variant}
      @about-symbol{stretch}
      @about-symbol{font-mask}
      @about-class{font-description}
      @about-function{font-description-new}
      @about-function{font-description-copy}
      @about-function{font-description-copy-static}
      @about-function{font-description-hash}
      @about-function{font-description-equal}
      @about-function{font-description-free}
      @about-function{font-descriptions-free}
      @about-function{font-description-family}
      @about-function{font-description-set-family-static}
      @about-function{font-description-style}
      @about-function{font-description-variant}
      @about-function{font-description-weight}
      @about-function{font-description-stretch}
      @about-function{font-description-size}
      @about-function{font-description-set-absolute-size}
      @about-function{font-description-size-is-absolute}
      @about-function{font-description-gravity}
      @about-function{font-description-variations}
      @about-function{font-description-set-variations-static}
      @about-function{font-description-set-fields}
      @about-function{font-description-unset-fields}
      @about-function{font-description-merge}
      @about-function{font-description-merge-static}
      @about-function{font-description-better-match}
      @about-function{font-description-from-string}
      @about-function{font-description-to-string}
      @about-function{font-description-to-filename}
      @about-class{font-metrics}
      @about-function{font-metrics-ref}
      @about-function{font-metrics-unref}
      @about-function{font-metrics-ascent}
      @about-function{font-metrics-descent}
      @about-function{font-metrics-height}
      @about-function{font-metrics-approximate-char-width}
      @about-function{font-metrics-approximate-digit-width}
      @about-function{font-metrics-underline-thickness}
      @about-function{font-metrics-underline-position}
      @about-function{font-metrics-strikethrough-thickness}
      @about-function{font-metrics-strikethrough-position}
      @about-class{font}
      @about-function{font-find-shaper}
      @about-function{font-describe}
      @about-function{font-describe-with-absolute-size}
      @about-function{font-face}
      @about-function{font-coverage}
      @about-function{font-has-char}
      @about-function{font-glyph-extents}
      @about-function{font-metrics}
      @about-function{font-font-map}
      @about-function{font-features}
      @about-function{font-hb-font}
      @about-class{font-family}
      @about-function{font-family-name}
      @about-function{font-family-is-monospace}
      @about-function{font-family-is-variable}
      @about-function{font-family-list-faces}
      @about-function{font-family-face}
      @about-class{font-face}
      @about-function{font-face-face-name}
      @about-function{font-face-list-sizes}
      @about-function{font-face-describe}
      @about-function{font-face-is-synthesized}
      @about-function{font-face-family}
      @about-class{font-map}
      @about-function{font-map-create-context}
      @about-function{font-map-load-font}
      @about-function{font-map-load-fontset}
      @about-function{font-map-list-families}
      @about-function{font-map-family}
      @about-function{font-map-serial}
      @about-function{font-map-changed}
      @about-function{font-map-shape-engine-type}
      @about-class{fontset}
      @about-function{fontset-font}
      @about-function{fontset-metrics}
      @about-symbol{PangoFontsetForeachFunc}
      @about-function{fontset-foreach}
    @end{subsection}
    @begin[Glyph Storage]{subsection}
      Structures for storing information about glyphs.

      the function @fun{pango:shape} produces a string of glyphs which can be
      measured or drawn to the screen. The following structures are used to
      store information about glyphs.
      @about-variable{+pango-scale+}
      @about-symbol{rectangle}
      @about-struct{matrix}
      @about-function{matrix-xx}
      @about-function{matrix-xy}
      @about-function{matrix-yx}
      @about-function{matrix-yy}
      @about-function{matrix-x0}
      @about-function{matrix-y0}
      @about-function{matrix-new}
      @about-function{matrix-init}
      @about-function{matrix-copy}
      @about-function{matrix-free}
      @about-function{matrix-translate}
      @about-function{matrix-scale}
      @about-function{matrix-rotate}
      @about-function{matrix-concat}
      @about-function{matrix-transform-point}
      @about-function{matrix-transform-distance}
      @about-function{matrix-transform-rectangle}
      @about-function{matrix-transform-pixel-rectangle}
      @about-function{matrix-font-scale-factor}
      @about-function{matrix-font-scale-factors}
      @about-symbol{glyph}
      @about-function{PANGO_GLYPH_EMPTY}
      @about-function{PANGO_GLYPH_INVALID_INPUT}
      @about-function{PANGO_GLYPH_UNKNOWN_FLAG}
      @about-class{glyph-info}
      @about-class{glyph-geometry}
      @about-class{glyph-unit}
      @about-class{glyph-vis-attr}
      @about-class{glyph-string}
      @about-class{glyph-item}
      @about-class{glyph-item-iter}
      @about-function{PANGO_TYPE_GLYPH_STRING}
      @about-function{pixels}
      @about-function{PANGO_PIXELS_FLOOR}
      @about-function{PANGO_PIXELS_CEIL}
      @about-function{PANGO_UNITS_ROUND}
      @about-function{units-to-double}
      @about-function{units-from-double}
      @about-function{PANGO_ASCENT}
      @about-function{PANGO_DESCENT}
      @about-function{PANGO_LBEARING}
      @about-function{PANGO_RBEARING}
      @about-function{extents-to-pixels}
      @about-function{PANGO_GET_UNKNOWN_GLYPH}
      @about-function{glyph-string-new}
      @about-function{glyph-string-copy}
      @about-function{glyph-string-set-size}
      @about-function{glyph-string-free}
      @about-function{glyph-string-extents}
      @about-function{glyph-string-extents-range}
      @about-function{glyph-string-get-width}
      @about-function{glyph-string-index-to-x}
      @about-function{glyph-string-x-to-index}
      @about-function{glyph-string-get-logical-widths}
      @about-function{glyph-item-copy}
      @about-function{glyph-item-free}
      @about-function{glyph-item-split}
      @about-function{glyph-item-apply-attrs}
      @about-function{glyph-item-letter-space}
      @about-function{glyph-item-get-logical-widths}
      @about-function{glyph-item-iter-copy}
      @about-function{glyph-item-iter-free}
      @about-function{glyph-item-iter-init-start}
      @about-function{glyph-item-iter-init-end}
      @about-function{glyph-item-iter-next-cluster}
      @about-function{glyph-item-iter-prev-cluster}
    @end{subsection}
    @begin[Text Attributes]{subsection}
      Attributed text is used in a number of places in Pango. It is used as the
      input to the itemization process and also when creating a Pango layout.
      The data types and functions in this section are used to represent and
      manipulate sets of attributes applied to a portion of text.
      @about-variable{+pango-scale-xx-small+}
      @about-variable{+pango-scale-x-small+}
      @about-variable{+pango-scale-small+}
      @about-variable{+pango-scale-medium+}
      @about-variable{+pango-scale-large+}
      @about-variable{+pango-scale-x-large+}
      @about-variable{+pango-scale-xx-large+}
      @about-symbol{attr-type}
      @about-symbol{underline}
      @about-symbol{overline}
      @about-symbol{show-flags}
      @about-symbol{text-transform}
      @about-symbol{baseline-shift}
      @about-symbol{font-scale}
      @about-struct{color}
      @about-function{color-red}
      @about-function{color-green}
      @about-function{color-blue}
      @about-function{color-new}
      @about-function{color-copy}
      @about-function{color-parse}
      @about-function{color-parse-with-alpha}
      @about-function{color-to-string}
      @about-symbol{attr-class}
      @about-struct{attribute}
      @about-function{attribute-klass}
      @about-function{attribute-start-index}
      @about-function{attribute-end-index}
      @about-function{attribute-type}
      @about-function{attribute-new}
      @about-function{attribute-init}
      @about-function{attribute-copy}
      @about-function{attribute-equal}
      @about-function{attribute-destroy}
      @about-symbol{attr-string}
      @about-symbol{attr-language}
      @about-symbol{attr-color}
      @about-symbol{attr-int}
      @about-symbol{attr-float}
      @about-symbol{attr-font-desc}
      @about-symbol{attr-shape}
      @about-symbol{attr-size}
      @about-symbol{attr-font-features}
      @about-function{attr-language-new}
      @about-function{attr-family-new}
      @about-function{attr-style-new}
      @about-function{attr-variant-new}
      @about-function{attr-stretch-new}
      @about-function{attr-weight-new}
      @about-function{attr-size-new}
      @about-function{attr-size-new-absolute}
      @about-function{attr-font-desc-new}
      @about-function{attr-foreground-new}
      @about-function{attr-background-new}
      @about-function{attr-strikethrough-new}
      @about-function{attr-strikethrough-color-new}
      @about-function{attr-underline-new}
      @about-function{attr-underline-color-new}
      @about-function{attr-overline-new}
      @about-function{attr-overline-color-new}
      @about-function{attr-shape-new}
      @about-function{attr-shape-new-with-data}
      @about-function{PangoAttrDataCopyFunc}
      @about-function{attr-scale-new}
      @about-function{attr-rise-new}
      @about-function{attr-letter-spacing-new}
      @about-function{attr-fallback-new}
      @about-function{attr-gravity-new}
      @about-function{attr-gravity-hint-new}
      @about-function{attr-font-features-new}
      @about-function{attr-foreground-alpha-new}
      @about-function{attr-background-alpha-new}
      @about-function{attr-allow-breaks-new}
      @about-function{attr-insert-hyphens-new}
      @about-function{attr-show-new}
      @about-function{attr-type-register}
      @about-function{attr-type-name}
      @about-class{attr-list}
      @about-function{attr-list-new}
      @about-function{attr-list-ref}
      @about-function{attr-list-unref}
      @about-function{attr-list-copy}
      @about-function{attr-list-insert}
      @about-function{attr-list-insert-before}
      @about-function{attr-list-change}
      @about-function{attr-list-splice}
      @about-function{attr-list-filter}
      @about-function{attr-list-update}
      @about-function{PangoAttrFilterFunc}
      @about-function{attr-list-attributes}
      @about-function{attr-list-equal}
      @about-function{attr-list-from-string}
      @about-function{attr-list-to-string}
      @about-function{attr-list-iterator}
      @about-class{attr-iterator}
      @about-function{attr-iterator-copy}
      @about-function{attr-iterator-next}
      @about-function{attr-iterator-range}
      @about-function{attr-iterator-get}
      @about-function{attr-iterator-font}
      @about-function{attr-iterator-attrs}
      @about-function{attr-iterator-destroy}
    @end{subsection}
    @begin[Pango Markup]{subsection}
      Simple markup language for text with attributes.

      Frequently, you want to display some text to the user with attributes
      applied to part of the text (for example, you might want bold or
      italicized words). With the base Pango interfaces, you could create a
      @class{pango:attr-list} instance and apply it to the text; the problem is
      that you'd need to apply attributes to some numeric range of characters,
      for example \"characters 12-17.\" This is broken from an
      internationalization standpoint; once the text is translated, the word
      you wanted to italicize could be in a different position.

      The solution is to include the text attributes in the string to be
      translated. Pango provides this feature with a small markup language.
      You can parse a marked-up string into the string text plus a
      @class{pango:attr-list} instance using either of the functions
      @fun{pango:parse-markup} or @fun{pango:markup-parser-new}.

      A simple example of a marked-up string might be:
      @begin{pre}
<span foreground=\"blue\" size=\"x-large\">Blue text</span> is <i>cool</i>!
      @end{pre}
      Pango uses GMarkup to parse this language, which means that XML features
      such as numeric character entities such as &amp;#169; for © can be used
      too.

      The root tag of a marked-up document is @code{<markup>}, but the function
      @fun{pango:parse-markup} allows you to omit this tag, so you will most
      likely never need to use it. The most general markup tag is @code{<span>},
      then there are some convenience tags.

      @subheading{Span attributes}
      @code{<span>} has the following attributes:
      @begin[code]{table}
        @entry[font_desc]{A font description string, such as \"Sans Italic 12\".
          See the function @fun{pango:font-description-from-string} for a
          description of the format of the string representation. Note that any
          other span attributes will override this description. So if you have
          \"Sans Italic\" and also a style=\"normal\" attribute, you will get
          Sans normal, not italic.}
        @entry[font_family]{A font family name.}
        @entry[font_size, size]{Font size in 1024ths of a point, or one of the
          absolute sizes xx-small, x-small, small, medium, large, x-large,
          xx-large, or one of the relative sizes smaller or larger. If you want
          to specify a absolute size, it's usually easier to take advantage of
          the ability to specify a partial font description using font; you can
          use @code{font='12.5'} rather than @code{size='12800'}.}
        @entry[font_style]{One of normal, oblique, italic.}
        @entry[font_weight]{One of ultralight, light, normal, bold, ultrabold,
          heavy, or a numeric weight.}
        @entry[font_variant]{One of normal or smallcaps.}
        @entry[font_stretch, stretch]{One of ultracondensed, extracondensed,
          condensed, semicondensed, normal, semiexpanded, expanded,
          extraexpanded, ultraexpanded.}
        @entry[font_features]{A comma-separated list of OpenType font feature
          settings, in the same syntax as accepted by CSS. E.g:
          @code{font_features='dlig=1, -kern, afrc on'}.}
        @entry[foreground, fgcolor]{An RGB color specification such as #00FF00
          or a color name such as red. Since 1.38, an RGBA color specification
          such as #00FF007F will be interpreted as specifying both a foreground
          color and foreground alpha.}
        @entry[background, bgcolor]{An RGB color specification such as #00FF00
          or a color name such as red. Since 1.38, an RGBA color specification
          such as #00FF007F will be interpreted as specifying both a background
          color and background alpha.}
        @entry[alpha, fgalpha]{An alpha value for the foreground color, either
          a plain integer between 1 and 65536 or a percentage value like 50%.}
        @entry[background_alpha, bgalpha]{An alpha value for the background
          color, either a plain integer between 1 and 65536 or a percentage
          value like 50%.}
        @entry[underline]{One of none, single, double, low, error, single-line,
          double-line or error-line.}
        @entry[underline_color]{The color of underlines; an RGB color
          specification such as #00FF00 or a color name such as red.}
        @entry[overline]{One of none or single.}
        @entry[overline_color]{The color of overlines; an RGB color
          specification such as #00FF00 or a color name such as red.}
        @entry[rise]{Vertical displacement, in Pango units. Can be negative for
          subscript, positive for superscript.}
        @entry[strikethrough]{true or false whether to strike through the text.}
        @entry[strikethrough_color]{The color of strikethrough lines; an RGB
          color specification such as #00FF00 or a color name such as red.}
        @entry[fallback]{true or false whether to enable fallback. If disabled,
          then characters will only be used from the closest matching font on
          the system. No fallback will be done to other fonts on the system that
          might contain the characters in the text. Fallback is enabled by
          default. Most applications should not disable fallback.}
        @entry[allow_breaks]{true or false whether to allow line breaks or not.
          If not allowed, the range will be kept in a single run as far as
          possible. Breaks are allowed by default.}
        @entry[insert_hyphens]{true or false` whether to insert hyphens when
          breaking lines in the middle of a word. Hyphens are inserted by
          default.}
        @entry[show]{A value determining how invisible characters are treated.
          Possible values are spaces, line-breaks, ignorables or combinations,
          such as spaces|line-breaks.}
        @entry[lang]{A language code, indicating the text language.}
        @entry[letter_spacing]{Inter-letter spacing in 1024ths of a point.}
        @entry[gravity]{One of south, east, north, west, auto.}
        @entry[gravity_hint]{One of natural, strong, line.}
      @end{table}
      @subheading{Convenience tags}
      The following convenience tags are provided:
      @begin[code]{table}
        @entry[<b>]{Bold.}
        @entry[<big>]{Makes font relatively larger, equivalent to
          @code{<span size=\"larger\">}.}
        @entry[<i>]{{Italic.}
        @entry[<s>]{Strikethrough.}
        @entry[<sub>]{Subscript.}
        @entry[<sup>]{Superscript.}
        @entry[<small>]{Makes font relatively smaller, equivalent to
          @code{<span size=\"smaller\">}.}
        @entry[<tt>]{Monospace.}
        @entry[<u>]{Underline.}
      @end{table}
      @about-function{parse-markup}
      @about-function{markup-parser-new}
      @about-function{markup-parser-finish}
    @end{subsection}
    @begin[Layout Objects]{subsection}
      High-level layout driver objects.

      While complete access to the layout capabilities of Pango is provided
      using the detailed interfaces for itemization and shaping, using that
      functionality directly involves writing a fairly large amount of code.
      The objects and functions in this section provide a high-level driver
      for formatting entire paragraphs of text at once. This includes
      paragraph-level functionality such as line-breaking, justification,
      alignment and ellipsization.
      @about-symbol{wrap-mode}
      @about-symbol{ellipsize-mode}
      @about-symbol{alignment}
      @about-symbol{layout-run}
      @about-class{layout}
      @about-function{layout-new}
      @about-function{layout-copy}
      @about-function{layout-context}
      @about-function{layout-context-changed}
      @about-function{layout-serial}
      @about-function{layout-text}
      @about-function{layout-character-count}
      @about-function{layout-set-markup}
      @about-function{layout-set-markup-with-accel}
      @about-function{layout-attributes}
      @about-function{layout-font-description}
      @about-function{layout-width}
      @about-function{layout-height}
      @about-function{layout-wrap}
      @about-function{layout-is-wrapped}
      @about-function{layout-ellipsize}
      @about-function{layout-is-ellipsized}
      @about-function{layout-indent}
      @about-function{layout-spacing}
      @about-function{layout-line-spacing}
      @about-function{layout-justify}
      @about-function{layout-auto-dir}
      @about-function{layout-direction}
      @about-function{layout-alignment}
      @about-function{layout-tabs}
      @about-function{layout-single-paragraph-mode}
      @about-function{layout-unknown-glyphs-count}
      @about-function{layout-log-attrs}
      @about-function{layout-log-attrs-readonly}
      @about-function{layout-index-to-pos}
      @about-function{layout-index-to-line-x}
      @about-function{layout-xy-to-index}
      @about-function{layout-cursor-pos}
      @about-function{layout-move-cursor-visually}
      @about-function{layout-extents}
      @about-function{layout-pixel-extents}
      @about-function{layout-size}
      @about-function{layout-pixel-size}
      @about-function{layout-baseline}
      @about-function{layout-line-count}
      @about-function{layout-line}
      @about-function{layout-line-readonly}
      @about-function{layout-lines}
      @about-function{layout-lines-readonly}
      @about-class{layout-iter}
      @about-function{layout-iter}
      @about-function{layout-iter-copy}
      @about-function{layout-iter-free}
      @about-function{layout-iter-next-run}
      @about-function{layout-iter-next-char}
      @about-function{layout-iter-next-cluster}
      @about-function{layout-iter-next-line}
      @about-function{layout-iter-at-last-line}
      @about-function{layout-iter-index}
      @about-function{layout-iter-baseline}
      @about-function{layout-iter-run}
      @about-function{layout-iter-run-readonly}
      @about-function{layout-iter-line}
      @about-function{layout-iter-line-readonly}
      @about-function{layout-iter-layout}
      @about-function{layout-iter-char-extents}
      @about-function{layout-iter-cluster-extents}
      @about-function{layout-iter-run-extents}
      @about-function{layout-iter-line-yrange}
      @about-function{layout-iter-line-extents}
      @about-function{layout-iter-layout-extents}
      @about-class{layout-line}
      @about-function{layout-line-ref}
      @about-function{layout-line-unref}
      @about-function{layout-line-extents}
      @about-function{layout-line-height}
      @about-function{layout-line-length}
      @about-function{layout-line-pixel-extents}
      @about-function{layout-line-resolved-direction}
      @about-function{layout-line-start-index}
      @about-function{layout-line-x-ranges}
      @about-function{layout-line-index-to-x}
      @about-function{layout-line-is-paragraph-start}
      @about-function{layout-line-x-to-index}
    @end{subsection}
    @begin[Scripts and Languages]{subsection}
      Identifying writing systems and languages.
      @about-symbol{script}
      @about-symbol{script-iter}
      @about-function{script-for-unichar}
      @about-function{script-sample-language}
      @about-function{script-iter-new}
      @about-function{script-iter-get-range}
      @about-function{script-iter-next}
      @about-function{script-iter-free}
      @about-class{language}
      @about-function{language-from-string}
      @about-function{language-to-string}
      @about-function{language-matches}
      @about-function{language-includes-script}
      @about-function{language-scripts}
      @about-function{language-default}
      @about-function{language-preferred}
      @about-function{language-sample-string}
    @end{subsection}
    @begin[Bidirectional Text]{subsection}
      Types and functions to help with handling bidirectional text.

      Pango supports bidirectional text (like Arabic and Hebrew) automatically.
      Some applications however, need some help to correctly handle
      bidirectional text.

      The @symbol{pango:direction} enumeration can be used with the function
      @fun{pango:context-base-dir} to instruct Pango about direction of text,
      though in most cases Pango detects that correctly and automatically.
      The rest of the facilities in this section are used internally by Pango
      already, and are provided to help applications that need more direct
      control over bidirectional setting of text.
      @about-symbol{direction}
      @about-symbol{bidi-type}
      @about-function{unichar-direction}
      @about-function{find-base-dir}
      @about-function{get-mirror-char}
      @about-function{bidi-type-for-unichar}
    @end{subsection}
    @begin[Vertical Text]{subsection}
      Laying text out in vertical directions.
      @about-symbol{gravity}
      @about-symbol{gravity-hint}
      @about-function{PANGO_GRAVITY_IS_IMPROPER}
      @about-function{PANGO_GRAVITY_IS_VERTICAL}
      @about-function{gravity-for-matrix}
      @about-function{gravity-for-script}
      @about-function{gravity-for-script-and-width}
      @about-function{gravity-to-rotation}
    @end{subsection}
  @end{section}
  @begin[Rendering with Pango]{section}
    @begin[Cairo Rendering]{subsection}
      The Cairo library is a vector graphics library with a powerful rendering
      model. It has such features as anti-aliased primitives, alpha-compositing,
      and gradients. Multiple backends for Cairo are available, to allow
      rendering to images, to PDF files, and to the screen on X and on other
      windowing systems. The functions in this section allow using Pango to
      render to Cairo surfaces.

      Using Pango with Cairo is straightforward. A @class{pango:context} object
      created with the @fun{pango:font-map-create-context} function can be used
      on any @symbol{cairo:context-t} instance, but needs to be updated to
      match the current transformation matrix and target surface of the Cairo
      context using the @fun{pango:cairo-update-context} function. The
      convenience @fun{pango:cairo-create-layout} and
      @fun{pango:cairo-update-layout} functions handle the common case where
      the program does not need to manipulate the properties of the
      @class{pango:context} object.

      When you get the metrics of a layout or of a piece of a layout using
      functions such as the @fun{pango:layout-extents} function, the reported
      metrics are in user-space coordinates. If a piece of text is 10 units
      long, and you call @code{(cairo:scale cr 2.0)}, it still is more-or-less
      10 units long. However, the results will be affected by hinting (that is,
      the process of adjusting the text to look good on the pixel grid), so you
      should not assume they are completely independent of the current
      transformation matrix. Note that the basic metrics functions in Pango
      report results in integer Pango units. To get to the floating point units
      used in Cairo divide by the @var{pango:+pango-scale+} value.
      @begin[Example]{dictionary}
        Using Pango with Cairo
        @begin{pre}
(defun draw-cairo-rendering (cr width height)
  (let ((radius (- (/ (min width height) 2) 20))
        (circle 260)
        (n-words 12)
        (font \"Sans Bold 16\"))
    ;; Set up a transformation matrix so that the user space
    ;; coordinates for where we are drawing are [-RADIUS, RADIUS],
    ;; [-RADIUS, RADIUS] We first center, then change the scale
    (cairo:translate cr (+ radius (/ (- width (* 2 radius)) 2))
                        (+ radius (/ (- height (* 2 radius)) 2)))
    (cairo:scale cr (/ radius circle) (/ radius circle))
    ;; Clear surface
    (cairo:set-source-rgb cr 1.0 1.0 1.0)
    (cairo:paint cr)
    ;; Create a PangoLayout, set the font and text
    (let ((layout (pango:cairo-create-layout cr))
          (desc (pango:font-description-from-string font)))
      (setf (pango:layout-text layout) \"Crategus\")
      (setf (pango:layout-font-description layout) desc)
      ;; Draw the layout n-words times in a circle
      (do* ((i 0 (+ i 1))
            (angle 0 (/ (* 360 i) n-words))
            ;; Gradient color
            (color (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)
                   (/ (+ 1 (cos (* (/ pi 180) (- angle 60)))) 2)))
           ((>= i n-words))
           (cairo:save cr)
           (cairo:set-source-rgb cr (/ #xFF 255) (/ #x99 255) color)
           (cairo:rotate cr (/ (* angle pi) 180))
           ;; Inform Pango to re-layout the text with the new
           ;; transformation matrix
           (pango:cairo-update-layout cr layout)
           (multiple-value-bind (width height)
               (pango:layout-size layout)
             (declare (ignore height))
             (cairo:move-to cr (- (/ width 2 +pango-scale+)) (- circle)))
             (pango:cairo-show-layout cr layout)
             (cairo:restore cr)))))
        @end{pre}
      @end{dictionary}
      @about-class{cairo-font}
      @about-class{cairo-font-map}
      @about-function{cairo-font-map-default}
      @about-function{cairo-font-map-new}
      @about-function{cairo-font-map-new-for-font-type}
      @about-function{cairo-font-map-font-type}
      @about-function{cairo-font-map-resolution}
      @about-function{cairo-font-map-create-context}
      @about-function{cairo-font-scaled-font}
      @about-function{cairo-context-resolution}
      @about-function{cairo-context-font-options}
      @about-symbol{cairo-shape-renderer-func}
      @about-function{cairo-context-shape-renderer}
      @about-function{cairo-create-context}
      @about-function{cairo-update-context}
      @about-function{cairo-create-layout}
      @about-function{cairo-update-layout}
      @about-function{cairo-show-glyph-string}
      @about-function{cairo-show-glyph-item}
      @about-function{cairo-show-layout-line}
      @about-function{cairo-show-layout}
      @about-function{cairo-show-error-underline}
      @about-function{cairo-glyph-string-path}
      @about-function{cairo-layout-line-path}
      @about-function{cairo-layout-path}
      @about-function{cairo-error-underline-path}
    @end{subsection}
    @begin[Win32 Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
    @begin[CoreText Fonts]{subsection}
      not implemented
    @end{subsection}
    @begin[FreeType Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
    @begin[Xft Fonts and Rendering]{subsection}
      not implemented
    @end{subsection}
  @end{section}
  @begin[Low Level Functionality]{section}
    @begin[Contexts]{subsection}
      Global context object.
      @about-class{context}
      @about-function{context-new}
      @about-function{context-changed}
      @about-function{context-serial}
      @about-function{context-font-map}
      @about-function{context-font-description}
      @about-function{context-language}
      @about-function{context-base-dir}
      @about-function{context-base-gravity}
      @about-function{context-gravity}
      @about-function{context-gravity-hint}
      @about-function{context-matrix}
      @about-function{context-round-glyph-positions}
      @about-function{context-load-font}
      @about-function{context-load-fontset}
      @about-function{context-metrics}
      @about-function{context-list-families}
    @end{subsection}
    @begin[Tab Stops]{subsection}
      Structures for storing tab stops.
      @about-class{tab-array}
      @about-symbol{tab-align}
      @about-function{tab-array-new}
      @about-function{tab-array-new-with-positions}
      @about-function{tab-array-copy}
      @about-function{tab-array-free}
      @about-function{tab-array-get-size}
      @about-function{tab-array-resize}
      @about-function{tab-array-set-tab}
      @about-function{tab-array-get-tab}
      @about-function{tab-array-get-tabs}
      @about-function{tab-array-get-positions-in-pixels}
    @end{subsection}
    @begin[Coverage Maps]{subsection}
      Unicode character range coverage storage.
      @about-symbol{coverage-level}
      @about-symbol{PANGO_TYPE_COVERAGE_LEVEL}
      @about-class{coverage}
      @about-function{coverage-new}
      @about-function{coverage-ref}
      @about-function{coverage-unref}
      @about-function{coverage-copy}
      @about-function{coverage-get}
      @about-function{coverage-max}
      @about-function{coverage-set}
      @about-function{coverage-to-bytes}
      @about-function{coverage-from-bytes}
    @end{subsection}
    @begin[PangoRenderer]{subsection}
      Rendering driver base class.
      @about-class{renderer}
      @about-symbol{render-part}
      @about-function{type-render-part}
      @about-class{renderer-class}
      @about-function{renderer-draw-layout}
      @about-function{renderer-draw-layout-line}
      @about-function{renderer-draw-glyphs}
      @about-function{renderer-draw-glyph-item}
      @about-function{renderer-draw-rectangle}
      @about-function{renderer-draw-error-underline}
      @about-function{renderer-draw-trapezoid}
      @about-function{renderer-draw-glyph}
      @about-function{renderer-activate}
      @about-function{renderer-deactivate}
      @about-function{renderer-part-changed}
      @about-function{renderer-set-color}
      @about-function{renderer-get-color}
      @about-function{renderer-set-alpha}
      @about-function{renderer-get-alpha}
      @about-function{renderer-set-matrix}
      @about-function{renderer-get-matrix}
      @about-function{renderer-get-layout}
      @about-function{renderer-get-layout-line}
    @end{subsection}
    @begin[PangoFcFontMap]{subsection}
      not implemented
    @end{subsection}
    @begin[PangoFcFont]{subsection}
      not implemented
    @end{subsection}
    @begin[PangoFcDecoder]{subsection}
      not implemented
    @end{subsection}
    @begin[Miscellaneous Utilities]{subsection}
      not implemented
    @end{subsection}
    @begin[Version Information]{subsection}
      Tools for checking Pango version at compile- and run-time.
      @about-function{PANGO_VERSION_ENCODE}
      @about-function{PANGO_VERSION}
      @about-function{PANGO_VERSION_MAJOR}
      @about-function{PANGO_VERSION_MINOR}
      @about-function{PANGO_VERSION_MICRO}
      @about-function{PANGO_VERSION_STRING}
      @about-function{PANGO_VERSION_CHECK}
      @about-function{version}
      @about-function{version-string}
      @about-function{version-check}
    @end{subsection}
  @end{section}
  @begin[Deprecated APIs]{section}
    @begin[OpenType Font Handling]{subsection}
      not implemented
    @end{subsection}
    @begin[Engines]{subsection}
      not implemented
    @end{subsection}
    @begin[PangoEngineLang]{subsection}
      not implemented
    @end{subsection}
    @begin[PangoEngineShape]{subsection}
      not implemented
    @end{subsection}
    @begin[Modules]{subsection}
      not implemented
    @end{subsection}
  @end{section}")

;;; --- End of file pango.package.lisp -----------------------------------------
