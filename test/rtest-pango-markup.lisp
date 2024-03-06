(in-package :pango-test)

(def-suite pango-markup :in pango-suite)
(in-suite pango-markup)

;;;     pango_parse_markup

(test pango-parse-markup.1
  (multiple-value-bind (text attrlist char)
      (pango:parse-markup "<b>_Text</b>" #\_)

    (is (string= "Text" text))
    (is (string= "0 1 underline low
0 4 weight bold"
                 (pango:attr-list-to-string attrlist)))
    (is (eq #\T char))))

(test pango-parse-markup.2
  (multiple-value-bind (text attrlist char)
      (pango:parse-markup "<span foreground='blue' size='x-large'>Blue text</span> is <i>cool</i>!" #\_)
    (is (string= "Blue text is cool!" text))
    (is (string= "0 9 scale 1.440000
0 9 foreground #00000000ffff
13 17 style italic"
                 (pango:attr-list-to-string attrlist)))
    (is (eq #\Nul char))))

;;;     pango_markup_parser_new
;;;     pango_markup_parser_finish

;;; 2024-2-27