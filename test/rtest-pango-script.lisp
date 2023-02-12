(in-package :pango-test)

(def-suite pango-script :in pango-suite)
(in-suite pango-script)

;;;     PangoLanguage

(test language-struct
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoLanguage") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoLanguage")
          (g:gtype (cffi:foreign-funcall "pango_language_get_type" :size)))))

;;;     pango_language_from_string
;;;     pango_language_to_string

(test language-from-string
  (let ((language (pango:language-from-string "de-DE")))
    (is (typep language 'pango:language))
    (is (string= "de-de" (pango:language-to-string language)))))

;;;     pango_language_matches

(test language-matches
  (let ((language (pango:language-from-string "de-DE")))
    (is-true (pango:language-matches language "de-de en-gb"))
    (is-false (pango:language-matches language "en-gb"))
    (is-true (pango:language-matches (pango:language-from-string "en_GB")
                                     "en-gb"))))

;;;     pango_language_includes_script
;;;     pango_language_get_scripts

;;;     pango_language_get_default

;; TODO: On Linux the default language is not initialized when loading the
;; Pango library.

#+windows
(test language-default
  (let ((language (pango:language-default)))
    (is (typep language 'pango:language))
    (is (string= "de-de" (pango:language-to-string language)))))

;;;     pango_language_get_preferred

(test language-preferred
  (is-false (pango:language-preferred)))

;;;     pango_language_get_sample_string

#-windows
(test language-sample-string
  (let ((language (pango:language-default)))
    (is (string= "The quick brown fox jumps over the lazy dog."
                 (pango:language-sample-string language)))
    (is (string= "The quick brown fox jumps over the lazy dog."
                 (pango:language-sample-string nil)))))

#+windows
(test language-sample-string
  (let ((language (pango:language-default)))
    (is (string= "Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich."
                 (pango:language-sample-string language)))
    (is (string= "Zwölf Boxkämpfer jagen Viktor quer über den großen Sylter Deich."
                 (pango:language-sample-string nil)))))

;;;     PangoScript
;;;     PangoScriptIter

;;;     pango_script_for_unichar
;;;     pango_script_get_sample_language
;;;     pango_script_iter_new
;;;     pango_script_iter_get_range
;;;     pango_script_iter_next
;;;     pango_script_iter_free

;;; --- 2023-2-6 ---------------------------------------------------------------
