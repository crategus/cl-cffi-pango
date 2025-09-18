;;; ----------------------------------------------------------------------------
;;; pango.script.lisp
;;;
;;; The documentation in this file is taken from the Pango Reference Manual
;;; version 1.56 and modified to document the Lisp binding to the Pango
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; Scripts and Languages
;;;
;;;     Identifying writing systems and languages
;;;
;;; Types and Values
;;;
;;;     PangoScript
;;;     PangoLanguage
;;;
;;;     PangoScriptIter                                     not implemented
;;;
;;; Functions
;;;
;;;     pango_language_from_string
;;;     pango_language_to_string
;;;     pango_language_matches
;;;     pango_language_includes_script
;;;     pango_language_get_scripts
;;;     pango_language_get_default
;;;     pango_language_get_preferred
;;;     pango_language_get_sample_string
;;;
;;;     pango_script_for_unichar                            Deprecated
;;;     pango_script_get_sample_language
;;;
;;;     pango_script_iter_new                               not implemented
;;;     pango_script_iter_get_range                         not implemented
;;;     pango_script_iter_next                              not implemented
;;;     pango_script_iter_free                              not implemented
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ├── PangoLanguage
;;;     ╰── PangoScriptIter
;;;
;;;     GEnum
;;;     ╰── PangoScript
;;;
;;; Description
;;;
;;;     The functions in this section are used to identify the writing system,
;;;     or script of individual characters and of ranges within a larger text
;;;     string.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoScript
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoScript" script
  (:export t
   :type-initializer "pango_script_get_type")
  (:INVALID-CODE -1)
  (:COMMON 0)
  (:INHERITED 1)
  (:ARABIC 2)
  (:ARMENIAN 3)
  (:BENGALI 4)
  (:BOPOMOFO 5)
  (:CHEROKEE 6)
  (:COPTIC 7)
  (:CYRILLIC 8)
  (:DESERET 9)
  (:DEVANAGARI 10)
  (:ETHIOPIC 11)
  (:GEORGIAN 12)
  (:GOTHIC 13)
  (:GREEK 14)
  (:GUJARATI 15)
  (:GURMUKHI 16)
  (:HAN 17)
  (:HANGUL 18)
  (:HEBREW 19)
  (:HIRAGANA 20)
  (:KANNADA 21)
  (:KATAKANA 22)
  (:KHMER 23)
  (:LAO 24)
  (:LATIN 25)
  (:MALAYALAM 26)
  (:MONGOLIAN 27)
  (:MYANMAR 28)
  (:OGHAM 29)
  (:OLD-ITALIC 30)
  (:ORIYA 31)
  (:RUNIC 32)
  (:SINHALA 33)
  (:SYRIAC 34)
  (:TAMIL 35)
  (:TELUGU 36)
  (:THAANA 37)
  (:THAI 38)
  (:TIBETAN 39)
  (:CANADIAN-ABORIGINAL 40)
  (:YI 41)
  (:TAGALOG 42)
  (:HANUNOO 43)
  (:BUHID 44)
  (:TAGBANWA 45)
  (:BRAILLE 46)
  (:CYPRIOT 47)
  (:LIMBU 48)
  (:OSMANYA 49)
  (:SHAVIAN 50)
  (:LINEAR-B 51)
  (:TAI-LE 52)
  (:UGARITIC 53)
  (:NEW-TAI-LUE 54)
  (:BUGINESE 55)
  (:GLAGOLITIC 56)
  (:TIFINAGH 57)
  (:SYLOTI-NAGRI 58)
  (:OLD-PERSIAN 59)
  (:KHAROSHTHI 60)
  (:UNKNOWN 61)
  (:BALINESE 62)
  (:CUNEIFORM 63)
  (:PHOENICIAN 64)
  (:PHAGS-PA 65)
  (:NKO 66)
  (:KAYAH-LI 67)
  (:LEPCHA 68)
  (:REJANG 69)
  (:SUNDANESE 70)
  (:SAURASHTRA 71)
  (:CHAM 72)
  (:OL-CHIKI 73)
  (:VAI 74)
  (:CARIAN 75)
  (:LYCIAN 76)
  (:LYDIAN 77)
  (:BATAK 78)
  (:BRAHMI 79)
  (:MANDAIC 80)
  (:CHAKMA 81)
  (:MEROITIC-CURSIVE 82)
  (:MEROITIC-HIEROGLYPHS 83)
  (:MIAO 84)
  (:SHARADA 85)
  (:SORA-SOMPENG 86)
  (:TAKRI 87)
  (:BASSA-VAH 88)
  (:CAUCASIAN-ALBANIAN 89)
  (:DUPLOYAN 90)
  (:ELBASAN 91)
  (:GRANTHA 92)
  (:KHOJKI 93)
  (:KHUDAWADI 94)
  (:LINEAR-A 95)
  (:MAHAJANI 96)
  (:MANICHAEAN 97)
  (:MENDE-KIKAKUI 98)
  (:MODI 99)
  (:MRO 100)
  (:NABATAEAN 101)
  (:OLD-NORTH-ARABIAN 102)
  (:OLD-PERMIC 103)
  (:PAHAWH-HMONG 104)
  (:PALMYRENE 105)
  (:PAU-CIN-HAU 106)
  (:PSALTER-PAHLAVI 107)
  (:SIDDHAM 108)
  (:TIRHUTA 109)
  (:WARANG-CITI 110)
  (:AHOM 111)
  (:ANATOLIAN-HIEROGLYPHS 112)
  (:HATRAN 113)
  (:MULTANI 114)
  (:OLD-HUNGARIAN 115)
  (:SIGNWRITING 116))

#+liber-documentation
(setf (liber:alias-for-symbol 'script)
      "GEnum"
      (liber:symbol-documentation 'script)
 #+liber-documentation
 "@version{2025-08-24}
  @begin{declaration}
(gobject:define-genum \"PangoScript\" script
  (:export t
   :type-initializer \"pango_script_get_type\")
  (:INVALID-CODE -1)
  (:COMMON 0)
  (:INHERITED 1)
  (:ARABIC 2)
  (:ARMENIAN 3)
  (:BENGALI 4)
  (:BOPOMOFO 5)
  (:CHEROKEE 6)
  (:COPTIC 7)
  (:CYRILLIC 8)
  (:DESERET 9)
  (:DEVANAGARI 10)
  (:ETHIOPIC 11)
  (:GEORGIAN 12)
  (:GOTHIC 13)
  (:GREEK 14)
  (:GUJARATI 15)
  (:GURMUKHI 16)
  (:HAN 17)
  (:HANGUL 18)
  (:HEBREW 19)
  (:HIRAGANA 20)
  (:KANNADA 21)
  (:KATAKANA 22)
  (:KHMER 23)
  (:LAO 24)
  (:LATIN 25)
  (:MALAYALAM 26)
  (:MONGOLIAN 27)
  (:MYANMAR 28)
  (:OGHAM 29)
  (:OLD-ITALIC 30)
  (:ORIYA 31)
  (:RUNIC 32)
  (:SINHALA 33)
  (:SYRIAC 34)
  (:TAMIL 35)
  (:TELUGU 36)
  (:THAANA 37)
  (:THAI 38)
  (:TIBETAN 39)
  (:CANADIAN-ABORIGINAL 40)
  (:YI 41)
  (:TAGALOG 42)
  (:HANUNOO 43)
  (:BUHID 44)
  (:TAGBANWA 45)
  (:BRAILLE 46)
  (:CYPRIOT 47)
  (:LIMBU 48)
  (:OSMANYA 49)
  (:SHAVIAN 50)
  (:LINEAR-B 51)
  (:TAI-LE 52)
  (:UGARITIC 53)
  (:NEW-TAI-LUE 54)
  (:BUGINESE 55)
  (:GLAGOLITIC 56)
  (:TIFINAGH 57)
  (:SYLOTI-NAGRI 58)
  (:OLD-PERSIAN 59)
  (:KHAROSHTHI 60)
  (:UNKNOWN 61)
  (:BALINESE 62)
  (:CUNEIFORM 63)
  (:PHOENICIAN 64)
  (:PHAGS-PA 65)
  (:NKO 66)
  (:KAYAH-LI 67)
  (:LEPCHA 68)
  (:REJANG 69)
  (:SUNDANESE 70)
  (:SAURASHTRA 71)
  (:CHAM 72)
  (:OL-CHIKI 73)
  (:VAI 74)
  (:CARIAN 75)
  (:LYCIAN 76)
  (:LYDIAN 77)
  (:BATAK 78)
  (:BRAHMI 79)
  (:MANDAIC 80)
  (:CHAKMA 81)
  (:MEROITIC-CURSIVE 82)
  (:MEROITIC-HIEROGLYPHS 83)
  (:MIAO 84)
  (:SHARADA 85)
  (:SORA-SOMPENG 86)
  (:TAKRI 87)
  (:BASSA-VAH 88)
  (:CAUCASIAN-ALBANIAN 89)
  (:DUPLOYAN 90)
  (:ELBASAN 91)
  (:GRANTHA 92)
  (:KHOJKI 93)
  (:KHUDAWADI 94)
  (:LINEAR-A 95)
  (:MAHAJANI 96)
  (:MANICHAEAN 97)
  (:MENDE-KIKAKUI 98)
  (:MODI 99)
  (:MRO 100)
  (:NABATAEAN 101)
  (:OLD-NORTH-ARABIAN 102)
  (:OLD-PERMIC 103)
  (:PAHAWH-HMONG 104)
  (:PALMYRENE 105)
  (:PAU-CIN-HAU 106)
  (:PSALTER-PAHLAVI 107)
  (:SIDDHAM 108)
  (:TIRHUTA 109)
  (:WARANG-CITI 110)
  (:AHOM 111)
  (:ANATOLIAN-HIEROGLYPHS 112)
  (:HATRAN 113)
  (:MULTANI 114)
  (:OLD-HUNGARIAN 115)
  (:SIGNWRITING 116))
  @end{declaration}
  @begin{short}
    The @sym{pango:script} enumeration identifies different writing systems.
  @end{short}
  The values correspond to the names as defined in the Unicode standard. See
  @url[http://www.unicode.org/reports/tr24/]{Unicode Standard Annex 24}.

  Note that this enumeration is deprecated and will not be updated to include
  values in newer versions of the Unicode standard. Applications should use
  the @code{GUnicodeScript} enumeration instead, whose values are
  interchangeable with the @sym{pango:script} enumeration.")

;;; ----------------------------------------------------------------------------
;;; PangoLanguage
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque language "PangoLanguage"
  :export t
  :type-initializer "pango_language_get_type"
  :alloc (error "PangoLanguage cannot be created from the Lisp side."))

#+liber-documentation
(setf (liber:alias-for-class 'language)
      "GBoxed"
      (documentation 'language 'type)
 "@version{2025-08-24}
  @begin{declaration}
(glib:define-gboxed-opaque language \"PangoLanguage\"
  :export t
  :type-initializer \"pango_language_get_type\"
  :alloc (error \"PangoLanguage cannot be created from the Lisp side.\"))
  @end{declaration}
  @begin{short}
    The @sym{pango:language} structure is used to represent a language.
  @end{short}
  It is opaque, and has no user visible fields.
  @see-constructor{pango:language-from-string}
  @see-function{pango:language-default}")

;;; ----------------------------------------------------------------------------
;;; pango_language_from_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_language_from_string" language-from-string)
    (g:boxed language :return)
 #+liber-documentation
 "@version{2025-02-15}
  @argument[language]{a string representing a language tag}
  @return{The newly created @class{pango:language} instance.}
  @begin{short}
    Takes a RFC-3066 format language tag as a string and convert it to a
    @class{pango:language} instance that can be efficiently copied and
    compared with other language tags.
  @end{short}
  This function first canonicalizes the string by converting it to lowercase,
  mapping @kbd{_} to @kbd{-}, and stripping all characters other than letters
  and @kbd{-}.

  Use the @fun{pango:language-default} function if you want to get the
  @class{pango:language} instance for the current locale of the process.
  @begin[Examples]{dictionary}
    @begin{pre}
(pango:language-from-string \"de-de\") => #<PANGO-LANGUAGE {1006D76393@}>
(pango:language-to-string *) => \"de-de\"
    @end{pre}
  @end{dictionary}
  @see-class{pango:language}
  @see-function{pango:language-default}"
  (language :string))

(export 'language-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_language_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_language_to_string" language-to-string) :string
 #+liber-documentation
 "@version{2025-02-15}
  @argument[language]{a @class{pango:language} instance}
  @return{The string representing the Pango language tag.}
  @begin{short}
    Gets the RFC-3066 format string representing the given Pango language tag.
  @end{short}
  @see-class{pango:language}
  @see-function{pango:language-from-string}"
  (language (g:boxed language)))

(export 'language-to-string)

;;; ----------------------------------------------------------------------------
;;; pango_language_matches
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_language_matches" language-matches) :boolean
 #+liber-documentation
 "@version{2025-02-15}
  @argument[language]{a @class{pango:language} instance}
  @argument[range]{a list of language ranges, separated by @kbd{; : ,} or
  space characters, each element must either be @kbd{*} or a RFC 3066 language
    range canonicalized as by the @fun{pango:language-from-string} function}
  @return{@em{True} if a match was found.}
  @begin{short}
    Checks if a language tag matches one of the elements in a list of language
    ranges.
  @end{short}
  A language tag is considered to match a range in the list if the range is
  @kbd{*}, the range is exactly the tag, or the range is a prefix of the tag,
  and the character after it in the tag is @kbd{-}.
  @begin[Examples]{dictionary}
    @begin{pre}
(pango:language-matches (pango:language-default) \"de-de en-gb\") => T
(pango:language-matches (pango:language-default) \"en-gb\") => NIL
    @end{pre}
  @end{dictionary}
  @see-class{pango:language}
  @see-function{pango:language-from-string}"
  (language (g:boxed language))
  (range :string))

(export 'language-matches)

;;; ----------------------------------------------------------------------------
;;; pango_language_includes_script
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_language_includes_script" %language-includes-script)
    :boolean
  (language (g:boxed language))
  (script script))

(defun language-includes-script (language script)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[language]{a @class{pango:language} instance}
  @argument[script]{a @sym{pango:script} value}
  @begin{return}
    @em{True} if @arg{script} is one of the scripts used to write @arg{language}
    or @code{nil}, if nothing is known about @arg{language}, including the case
    that @arg{language} is @code{nil}.
  @end{return}
  @begin{short}
    Determines if @arg{script} is one of the scripts used to write
    @arg{language}.
  @end{short}
  The returned value is conservative. If nothing is known about the language
  tag @arg{language}, @em{true} will be returned, since, as far as Pango knows,
  @arg{script} might be used to write @arg{language}.

  This routine is used in the itemization process of Pango when determining if a
  supplied language tag is relevant to a particular section of text. It probably
  is not useful for applications in most circumstances.

  This function uses the @fun{pango:language-scripts} function internally.
  @see-class{pango:language}
  @see-symbol{pango:script}
  @see-function{pango:language-scripts}"
  (when language
    (%language-includes-script language script)))

(export 'language-includes-script)

;;; ----------------------------------------------------------------------------
;;; pango_language_get_scripts
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_language_get_scripts" %language-scripts) :pointer
  (language (g:boxed language))
  (num (:pointer :int)))

(defun language-scripts (language)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[language]{a @class{pango:language} instance}
  @begin{return}
    The list with @sym{pango:script} values, or @code{nil} if Pango does not
    have any information about this particular language tag, also the case if
    @arg{language} is @code{nil}.
  @end{return}
  @begin{short}
    Determines the scripts used to to write @arg{language}.
  @end{short}
  If nothing is known about the language tag @arg{language}, or if
  @arg{language} is @code{nil}, then @code{nil} is returned. The list of scripts
  returned starts with the script that the language uses most and continues to
  the one it uses least.

  Most languages use only one script for writing, but there are some that use
  two (Latin and Cyrillic for example), and a few use three, Japanese for
  example. Applications should not make any assumptions on the maximum number
  of scripts returned though, except that it is positive if the return value
  is not @code{nil}, and it is a small number.
  @see-class{pango:language}
  @see-symbol{pango:script}"
  (when language
    (cffi:with-foreign-object (num :int)
      (let ((scripts (%language-scripts language num)))
        (unless (cffi:null-pointer-p scripts)
          (iter (for i from 0 below (cffi:mem-ref num :int))
                (collect (cffi:mem-aref scripts 'script i))))))))

(export 'language-scripts)

;;; ----------------------------------------------------------------------------
;;; pango_language_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_language_get_default" language-default) (g:boxed language)
 #+liber-documentation
 "@version{2025-02-15}
  @return{The default language as a @class{pango:language} instance.}
  @begin{short}
    Returns the @class{pango:language} instance for the current locale of the
    process.
  @end{short}
  Note that this can change over the life of an application.

  On Unix systems, this is the return value derived from
  @code{setlocale(LC_CTYPE, NULL)}, and the user can affect this through the
  environment variables @code{LC_ALL}, @code{LC_CTYPE} or @code{LANG} (checked
  in that order). The locale string typically is in the form
  @code{lang_COUNTRY}, where @code{lang} is an ISO-639 language code, and
  @code{COUNTRY} is an ISO-3166 country code. For instance, @code{sv_FI} for
  Swedish as written in Finland or @code{pt_BR} for Portuguese as written in
  Brazil.

  On Windows, the C library does not use any such environment variables, and
  setting them will not affect the behavior of functions like @code{ctime()}.
  The user sets the locale through the Regional Options in the Control Panel.
  The C library, in the @code{setlocale()} function, does not use country and
  language codes, but country and language names spelled out in English.
  However, this function does check the above environment variables, and does
  return a Unix-style locale string based on either said environment variables
  or the thread's current locale.

  Your application should call @code{setlocale(LC_ALL, \"\");} for the user
  settings to take effect. GTK does this in its initialization functions
  automatically by calling @code{gtk_set_locale()}.
  @begin[Example]{dictionary}
    @begin{pre}
(pango:language-default) => #<PANGO-LANGUAGE {10019E6973@}>
(pango:language-to-string *) => \"de-de\"
    @end{pre}
  @end{dictionary}
  @see-class{pango:language}
  @see-function{pango:language-sample-string}")

(export 'language-default)

;;; ----------------------------------------------------------------------------
;;; pango_language_get_preferred
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_language_get_preferred" %language-preferred) :pointer)

(defun language-preferred ()
 #+liber-documentation
 "@version{2025-02-15}
  @return{The list of @class{pango:language} instances, or @code{nil}.}
  @begin{short}
    Returns the list of languages that the user prefers, as specified by the
     @code{PANGO_LANGUAGE} or @code{LANGUAGE} environment variables, in order
     of preference.
  @end{short}
  Note that this list does not necessarily include the language returned by
  the @fun{pango:language-default} function.

  When choosing language specific resources, such as the sample text returned
  by the @fun{pango:language-sample-string} function, you should first try the
  default language, followed by the languages returned by this function.
  @see-class{pango:language}
  @see-function{pango:language-default}
  @see-function{pango:language-sample-string}"
  (let (ptr)
    (unless (cffi:null-pointer-p (setf ptr (%language-preferred)))
      (iter (for count from 0)
            (for lang = (cffi:mem-aref ptr :pointer count))
            (until (cffi:null-pointer-p lang))
            (collect (cffi:mem-ref lang '(g:boxed language)))))))

(export 'language-preferred)

;;; ----------------------------------------------------------------------------
;;; pango_language_get_sample_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_language_get_sample_string" language-sample-string)
    :string
 #+liber-documentation
 "@version{2025-02-15}
  @argument[language]{a @class{pango:language} instance, or @code{nil}}
  @return{The sample string.}
  @begin{short}
    Get a string that is representative of the characters needed to render
    a particular language.
  @end{short}
  The sample text may be a pangram, but is not necessarily. It is chosen to
  be demonstrative of normal text in the language, as well as exposing font
  feature requirements unique to the language. It is suitable for use as
  sample text in a font selection dialog.

  If @arg{language} is @code{nil}, the default language as found by the
  @fun{pango:language-default} function is used.

  If Pango does not have a sample string for @arg{language}, the classic
  \"The quick brown fox ...\" is returned.
  @begin[Example]{dictionary}
    @begin{pre}
(pango:language-sample-string (pango:language-default))
=> \"Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich.\"
    @end{pre}
  @end{dictionary}
  @see-class{pango:language}
  @see-function{pango:language-default}"
  (language (g:boxed language)))

(export 'language-sample-string)

;;; ----------------------------------------------------------------------------
;;; pango_script_for_unichar
;;;
;;; Warning
;;;
;;; pango_script_for_unichar has been deprecated since version 1.44. and should
;;; not be used in newly written code. Use g_unichar_get_script()
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_get_sample_language
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_script_get_sample_language" script-sample-language)
    (g:boxed language)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[script]{a @sym{pango:script} value}
  @begin{return}
    The @class{pango:language} instance that is representative of the script,
    or @code{nil} if no such language exists.
  @end{return}
  @begin{short}
    Given a script, finds a language tag that is reasonably representative of
    that script.
  @end{short}
  This will usually be the most widely spoken or used language written in that
  script: for instance, the sample language for the
  @val[pango:script]{:cyrillic} value is ru (Russian), the sample language for
  the @val[pango:script]{:arabic} value is ar.

  For some scripts, no sample language will be returned because there is no
  language that is sufficiently representative. The best example of this is the
  @val[pango:script]{:han} value, where various different variants of written
  Chinese, Japanese, and Korean all use significantly different sets of Han
  characters and forms of shared characters. No sample language can be provided
  for many historical scripts as well.

  This function checks the environment variables @code{PANGO_LANGUAGE} and
  @code{LANGUAGE}, checked in that order, first. If one of them is set, it is
  parsed as a list of language tags separated by colons or other separators.
  This function will return the first language in the parsed list that Pango
  believes may use @arg{script} for writing. This last predicate is tested using
  the @fun{pango:language-includes-script} function. This can be used to control
  Pango's font selection for non-primary languages. For example, a
  @code{PANGO_LANGUAGE} enviroment variable set to \"en:fa\" makes Pango choose
  fonts suitable for Persian (fa) instead of Arabic (ar) when a segment of
  Arabic text is found in an otherwise non-Arabic text. The same trick can be
  used to choose a default language for @code{PANGO_SCRIPT_HAN} when setting
  context language is not feasible.
  @see-class{pango:language}
  @see-symbol{pango:script}
  @see-function{pango:language-includes-script}"
  (script script))

(export 'script-sample-language)

;;; ----------------------------------------------------------------------------
;;; PangoScriptIter
;;;
;;; typedef struct _PangoScriptIter PangoScriptIter;
;;;
;;; A PangoScriptIter is used to iterate through a string and identify ranges
;;; in different scripts.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_iter_new ()
;;;
;;; PangoScriptIter * pango_script_iter_new (const char *text, int length);
;;;
;;; Create a new PangoScriptIter, used to break a string of Unicode into runs
;;; by text. No copy is made of text, so the caller needs to make sure it
;;; remains valid until the iterator is freed with pango_script_iter_free().
;;;
;;; text :
;;;     a UTF-8 string
;;;
;;; length :
;;;     length of text, or -1 if text is nul-terminated
;;;
;;; Returns :
;;;     The new script iterator, initialized to point at the first range in the
;;      text, which should be freed with pango_script_iter_free(). If the string
;;;     is empty, it will point at an empty range.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_iter_get_range ()
;;;
;;; void pango_script_iter_get_range (PangoScriptIter *iter,
;;;                                   const char **start,
;;;                                   const char **end,
;;;                                   PangoScript *script);
;;;
;;; Gets information about the range to which iter currently points. The range
;;; is the set of locations p where *start <= p < *end. (That is, it does not
;;; include the character stored at *end)
;;;
;;; iter :
;;;     a PangoScriptIter
;;;
;;; start :
;;;     location to store start position of the range, or NULL
;;;
;;; end :
;;;     location to store end position of the range, or NULL
;;;
;;; script :
;;;     location to store script for range, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_iter_next ()
;;;
;;; gboolean pango_script_iter_next (PangoScriptIter *iter);
;;;
;;; Advances a PangoScriptIter to the next range. If iter is already at the end,
;;; it is left unchanged and FALSE is returned.
;;;
;;; iter :
;;;     a PangoScriptIter
;;;
;;; Returns :
;;;     TRUE if iter was successfully advanced
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_script_iter_free ()
;;;
;;; void pango_script_iter_free (PangoScriptIter *iter);
;;;
;;; Frees a PangoScriptIter created with pango_script_iter_new().
;;;
;;; iter :
;;;     a PangoScriptIter
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.script.lisp ------------------------------------------
