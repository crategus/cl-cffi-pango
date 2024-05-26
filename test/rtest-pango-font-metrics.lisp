(in-package :pango-test)

(def-suite pango-font-metrics-suite :in pango-suite)
(in-suite pango-font-metrics-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoFontMetrics

(test pango-font-metrics-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoFontMetrics"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoFontMetrics")
          (g:gtype (cffi:foreign-funcall "pango_font_metrics_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:font-metrics
          (glib:symbol-for-gtype "PangoFontMetrics"))))

;;;     pango_font_metrics_get_ascent
;;;     pango_font_metrics_get_descent
;;;     pango_font_metrics_get_height
;;;     pango_font_metrics_get_approximate_char_width
;;;     pango_font_metrics_get_approximate_digit_width
;;;     pango_font_metrics_get_underline_thickness
;;;     pango_font_metrics_get_underline_position
;;;     pango_font_metrics_get_strikethrough_thickness
;;;     pango_font_metrics_get_strikethrough_position

(test pango-font-metrics.1
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (lang (pango:language-from-string "de-de"))
         (metrics (pango:context-metrics context desc lang)))
    (is (typep metrics 'pango:font-metrics))
    (is (integerp (pango:font-metrics-ascent metrics)))
    (is (integerp (pango:font-metrics-descent metrics)))
    (is (integerp (pango:font-metrics-height metrics)))
    (is (integerp (pango:font-metrics-approximate-char-width metrics)))
    (is (integerp (pango:font-metrics-approximate-digit-width metrics)))
    (is (integerp (pango:font-metrics-underline-thickness metrics)))
    (is (integerp (pango:font-metrics-underline-position metrics)))
    (is (integerp (pango:font-metrics-strikethrough-thickness metrics)))
    (is (integerp (pango:font-metrics-strikethrough-position metrics)))))

#+crategus
(test pango-font-metrics.2
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (lang (pango:language-from-string "de-de"))
         (metrics (pango:context-metrics context desc lang)))
    (is (typep metrics 'pango:font-metrics))
    (is (= 18432 (pango:font-metrics-ascent metrics)))
    (is (=  5120 (pango:font-metrics-descent metrics)))
    (is (= 22528 (pango:font-metrics-height metrics)))
    (is (=  7996 (pango:font-metrics-approximate-char-width metrics)))
    (is (=  9216 (pango:font-metrics-approximate-digit-width metrics)))
    (is (=  1024 (pango:font-metrics-underline-thickness metrics)))
    (is (= -1024 (pango:font-metrics-underline-position metrics)))
    (is (=  1024 (pango:font-metrics-strikethrough-thickness metrics)))
    (is (=  6144 (pango:font-metrics-strikethrough-position metrics)))))

#+windows
(test pango-font-metrics.2
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 12"))
         (lang (pango:language-from-string "de-de"))
         (metrics (pango:context-metrics context desc lang)))
    (is (typep metrics 'pango:font-metrics))
    (is (= 15360 (pango:font-metrics-ascent metrics)))
    (is (=  4096 (pango:font-metrics-descent metrics)))
    (is (= 19456 (pango:font-metrics-height metrics)))
    (is (=  8553 (pango:font-metrics-approximate-char-width metrics)))
    (is (= 10240 (pango:font-metrics-approximate-digit-width metrics)))
    (is (=  1024 (pango:font-metrics-underline-thickness metrics)))
    (is (= -2048 (pango:font-metrics-underline-position metrics)))
    (is (=  1024 (pango:font-metrics-strikethrough-thickness metrics)))
    (is (=  5120 (pango:font-metrics-strikethrough-position metrics)))))

#+crategus
(test pango-font-metrics.3
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 18"))
         (lang (pango:language-from-string "de-de"))
         (metrics (pango:context-metrics context desc lang)))
    (is (typep metrics 'pango:font-metrics))
    (is (= 26624 (pango:font-metrics-ascent metrics)))
    (is (=  8192 (pango:font-metrics-descent metrics)))
    (is (= 33792 (pango:font-metrics-height metrics)))
    (is (= 11995 (pango:font-metrics-approximate-char-width metrics)))
    (is (= 14336 (pango:font-metrics-approximate-digit-width metrics)))
    (is (=  2048 (pango:font-metrics-underline-thickness metrics)))
    (is (= -2048 (pango:font-metrics-underline-position metrics)))
    (is (=  2048 (pango:font-metrics-strikethrough-thickness metrics)))
    (is (=  8192 (pango:font-metrics-strikethrough-position metrics)))))

#+windows
(test pango-font-metrics.3
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (desc (pango:font-description-from-string "Sans 18"))
         (lang (pango:language-from-string "de-de"))
         (metrics (pango:context-metrics context desc lang)))
    (is (typep metrics 'pango:font-metrics))
    (is (= 22528 (pango:font-metrics-ascent metrics)))
    (is (=  6144 (pango:font-metrics-descent metrics)))
    (is (= 28672 (pango:font-metrics-height metrics)))
    (is (= 12829 (pango:font-metrics-approximate-char-width metrics)))
    (is (= 15360 (pango:font-metrics-approximate-digit-width metrics)))
    (is (=  2048 (pango:font-metrics-underline-thickness metrics)))
    (is (= -2048 (pango:font-metrics-underline-position metrics)))
    (is (=  2048 (pango:font-metrics-strikethrough-thickness metrics)))
    (is (=  7168 (pango:font-metrics-strikethrough-position metrics)))))

;;; 2024-5-25
