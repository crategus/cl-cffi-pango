(in-package :pango-test)

(def-suite pango-version-suite :in pango-suite)
(in-suite pango-version-suite)

;;;     pango_version

#-windows
(test pango-version
  (is (= 15201 (pango:version))))

#+windows
(test pango-version
  (is (= 15014 (pango:version))))

;;;     pango_version_string

#-windows
(test pango-version-string
  (is (string= "1.52.1" (pango:version-string))))

#+windows
(test pango-version-string
  (is (string= "1.50.14" (pango:version-string))))

;;;     pango_version_check

(test pango-version-check
  (is-false (pango:version-check 1 48 0))
  (is (string= "Pango version too old (micro mismatch)"
               (pango:version-check 1 54 0))))

;;; 2024-5-25
