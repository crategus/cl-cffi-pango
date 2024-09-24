(in-package :pango-test)

(def-suite pango-script-suite :in pango-suite)
(in-suite pango-script-suite)

;;;     PangoScript

(test pango-script-enumeration
  ;; Check type
  (is (g:type-is-enum "PangoScript"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoScript")
          (g:gtype (cffi:foreign-funcall "pango_script_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:script
          (glib:symbol-for-gtype "PangoScript")))
  ;; Check names
  (is (equal '("PANGO_SCRIPT_INVALID_CODE" "PANGO_SCRIPT_COMMON"
               "PANGO_SCRIPT_INHERITED" "PANGO_SCRIPT_ARABIC"
               "PANGO_SCRIPT_ARMENIAN" "PANGO_SCRIPT_BENGALI"
               "PANGO_SCRIPT_BOPOMOFO" "PANGO_SCRIPT_CHEROKEE"
               "PANGO_SCRIPT_COPTIC" "PANGO_SCRIPT_CYRILLIC"
               "PANGO_SCRIPT_DESERET" "PANGO_SCRIPT_DEVANAGARI"
               "PANGO_SCRIPT_ETHIOPIC" "PANGO_SCRIPT_GEORGIAN"
               "PANGO_SCRIPT_GOTHIC" "PANGO_SCRIPT_GREEK"
               "PANGO_SCRIPT_GUJARATI" "PANGO_SCRIPT_GURMUKHI"
               "PANGO_SCRIPT_HAN" "PANGO_SCRIPT_HANGUL" "PANGO_SCRIPT_HEBREW"
               "PANGO_SCRIPT_HIRAGANA" "PANGO_SCRIPT_KANNADA"
               "PANGO_SCRIPT_KATAKANA" "PANGO_SCRIPT_KHMER" "PANGO_SCRIPT_LAO"
               "PANGO_SCRIPT_LATIN" "PANGO_SCRIPT_MALAYALAM"
               "PANGO_SCRIPT_MONGOLIAN" "PANGO_SCRIPT_MYANMAR"
               "PANGO_SCRIPT_OGHAM" "PANGO_SCRIPT_OLD_ITALIC"
               "PANGO_SCRIPT_ORIYA" "PANGO_SCRIPT_RUNIC" "PANGO_SCRIPT_SINHALA"
               "PANGO_SCRIPT_SYRIAC" "PANGO_SCRIPT_TAMIL" "PANGO_SCRIPT_TELUGU"
               "PANGO_SCRIPT_THAANA" "PANGO_SCRIPT_THAI" "PANGO_SCRIPT_TIBETAN"
               "PANGO_SCRIPT_CANADIAN_ABORIGINAL" "PANGO_SCRIPT_YI"
               "PANGO_SCRIPT_TAGALOG" "PANGO_SCRIPT_HANUNOO"
               "PANGO_SCRIPT_BUHID" "PANGO_SCRIPT_TAGBANWA"
               "PANGO_SCRIPT_BRAILLE" "PANGO_SCRIPT_CYPRIOT"
               "PANGO_SCRIPT_LIMBU" "PANGO_SCRIPT_OSMANYA"
               "PANGO_SCRIPT_SHAVIAN" "PANGO_SCRIPT_LINEAR_B"
               "PANGO_SCRIPT_TAI_LE" "PANGO_SCRIPT_UGARITIC"
               "PANGO_SCRIPT_NEW_TAI_LUE" "PANGO_SCRIPT_BUGINESE"
               "PANGO_SCRIPT_GLAGOLITIC" "PANGO_SCRIPT_TIFINAGH"
               "PANGO_SCRIPT_SYLOTI_NAGRI" "PANGO_SCRIPT_OLD_PERSIAN"
               "PANGO_SCRIPT_KHAROSHTHI" "PANGO_SCRIPT_UNKNOWN"
               "PANGO_SCRIPT_BALINESE" "PANGO_SCRIPT_CUNEIFORM"
               "PANGO_SCRIPT_PHOENICIAN" "PANGO_SCRIPT_PHAGS_PA"
               "PANGO_SCRIPT_NKO" "PANGO_SCRIPT_KAYAH_LI"
               "PANGO_SCRIPT_LEPCHA" "PANGO_SCRIPT_REJANG"
               "PANGO_SCRIPT_SUNDANESE" "PANGO_SCRIPT_SAURASHTRA"
               "PANGO_SCRIPT_CHAM" "PANGO_SCRIPT_OL_CHIKI" "PANGO_SCRIPT_VAI"
               "PANGO_SCRIPT_CARIAN" "PANGO_SCRIPT_LYCIAN" "PANGO_SCRIPT_LYDIAN"
               "PANGO_SCRIPT_BATAK" "PANGO_SCRIPT_BRAHMI" "PANGO_SCRIPT_MANDAIC"
               "PANGO_SCRIPT_CHAKMA" "PANGO_SCRIPT_MEROITIC_CURSIVE"
               "PANGO_SCRIPT_MEROITIC_HIEROGLYPHS" "PANGO_SCRIPT_MIAO"
               "PANGO_SCRIPT_SHARADA" "PANGO_SCRIPT_SORA_SOMPENG"
               "PANGO_SCRIPT_TAKRI" "PANGO_SCRIPT_BASSA_VAH"
               "PANGO_SCRIPT_CAUCASIAN_ALBANIAN" "PANGO_SCRIPT_DUPLOYAN"
               "PANGO_SCRIPT_ELBASAN" "PANGO_SCRIPT_GRANTHA"
               "PANGO_SCRIPT_KHOJKI" "PANGO_SCRIPT_KHUDAWADI"
               "PANGO_SCRIPT_LINEAR_A" "PANGO_SCRIPT_MAHAJANI"
               "PANGO_SCRIPT_MANICHAEAN" "PANGO_SCRIPT_MENDE_KIKAKUI"
               "PANGO_SCRIPT_MODI" "PANGO_SCRIPT_MRO" "PANGO_SCRIPT_NABATAEAN"
               "PANGO_SCRIPT_OLD_NORTH_ARABIAN" "PANGO_SCRIPT_OLD_PERMIC"
               "PANGO_SCRIPT_PAHAWH_HMONG" "PANGO_SCRIPT_PALMYRENE"
               "PANGO_SCRIPT_PAU_CIN_HAU" "PANGO_SCRIPT_PSALTER_PAHLAVI"
               "PANGO_SCRIPT_SIDDHAM" "PANGO_SCRIPT_TIRHUTA"
               "PANGO_SCRIPT_WARANG_CITI" "PANGO_SCRIPT_AHOM"
               "PANGO_SCRIPT_ANATOLIAN_HIEROGLYPHS" "PANGO_SCRIPT_HATRAN"
               "PANGO_SCRIPT_MULTANI" "PANGO_SCRIPT_OLD_HUNGARIAN"
               "PANGO_SCRIPT_SIGNWRITING")
             (glib-test:list-enum-item-names "PangoScript")))
  ;; Check values
  (is (equal '(-1 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
               24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45
               46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67
               68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89
               90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108
               109 110 111 112 113 114 115 116)
             (glib-test:list-enum-item-values "PangoScript")))
  ;; Check nick names
  (is (equal '("invalid-code" "common" "inherited" "arabic" "armenian" "bengali"
               "bopomofo" "cherokee" "coptic" "cyrillic" "deseret" "devanagari"
               "ethiopic" "georgian" "gothic" "greek" "gujarati" "gurmukhi"
               "han" "hangul" "hebrew" "hiragana" "kannada" "katakana" "khmer"
               "lao" "latin" "malayalam" "mongolian" "myanmar" "ogham"
               "old-italic" "oriya" "runic" "sinhala" "syriac" "tamil" "telugu"
               "thaana" "thai" "tibetan" "canadian-aboriginal" "yi" "tagalog"
               "hanunoo" "buhid" "tagbanwa" "braille" "cypriot" "limbu"
               "osmanya" "shavian" "linear-b" "tai-le" "ugaritic" "new-tai-lue"
               "buginese" "glagolitic" "tifinagh" "syloti-nagri" "old-persian"
               "kharoshthi" "unknown" "balinese" "cuneiform" "phoenician"
               "phags-pa" "nko" "kayah-li" "lepcha" "rejang" "sundanese"
               "saurashtra" "cham" "ol-chiki" "vai" "carian" "lycian" "lydian"
               "batak" "brahmi" "mandaic" "chakma" "meroitic-cursive"
               "meroitic-hieroglyphs" "miao" "sharada" "sora-sompeng" "takri"
               "bassa-vah" "caucasian-albanian" "duployan" "elbasan" "grantha"
               "khojki" "khudawadi" "linear-a" "mahajani" "manichaean"
               "mende-kikakui" "modi" "mro" "nabataean" "old-north-arabian"
               "old-permic" "pahawh-hmong" "palmyrene" "pau-cin-hau"
               "psalter-pahlavi" "siddham" "tirhuta" "warang-citi" "ahom"
               "anatolian-hieroglyphs" "hatran" "multani" "old-hungarian"
               "signwriting")
             (glib-test:list-enum-item-nicks "PangoScript")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoScript" PANGO:SCRIPT
                                    (:EXPORT T
                                     :TYPE-INITIALIZER "pango_script_get_type")
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
             (gobject:get-gtype-definition "PangoScript"))))

;;;     PangoLanguage

(test pango-language-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoLanguage"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoLanguage")
          (g:gtype (cffi:foreign-funcall "pango_language_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:language
          (glib:symbol-for-gtype "PangoLanguage"))))

;;;     pango_language_from_string
;;;     pango_language_to_string

(test pango-language-from-string
  (let ((language (pango:language-from-string "de-DE")))
    (is (typep language 'pango:language))
    (is (string= "de-de" (pango:language-to-string language)))))

;;;     pango_language_matches

(test pango-language-matches
  (let ((language (pango:language-from-string "de-DE")))
    (is-true (pango:language-matches language "de-de en-gb"))
    (is-false (pango:language-matches language "en-gb"))
    (is-true (pango:language-matches (pango:language-from-string "en_GB")
                                     "en-gb"))))

;;;     pango_language_includes_script

(test pango-language-includes-script
  (let ((lang (pango:language-from-string "de-DE")))
    (is-true (pango:language-includes-script lang :latin))))

;;;     pango_language_get_scripts

(test pango-language-scripts
  (let ((lang (pango:language-from-string "de-DE")))
    (is (equal '(:latin) (pango:language-scripts lang)))))

;;;     pango_language_get_default

(test pango-language-default
  (let ((language (pango:language-default)))
    (is (typep language 'pango:language))
    (is (string= "de-de" (pango:language-to-string language)))))

;;;     pango_language_get_preferred

(test pango-language-preferred
  (is-false (pango:language-preferred)))

;;;     pango_language_get_sample_string

(test pango-language-sample-string
  (let ((language (pango:language-default)))
    (is (string= "Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich."
                 (pango:language-sample-string language)))
    (is (string= "Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich."
                 (pango:language-sample-string nil)))))

;;;     pango_script_for_unichar                          Deprecated

;;;     pango_script_get_sample_language

(test pango-script-sample-language
  (is (string= "en"
               (pango:language-to-string
                   (pango:script-sample-language :latin)))))

;;;     PangoScriptIter                                    not implemented
;;;     pango_script_iter_new                              not implemented
;;;     pango_script_iter_get_range                        not implemented
;;;     pango_script_iter_next                             not implemented
;;;     pango_script_iter_free                             not implemented

;;; 2024-9-18
