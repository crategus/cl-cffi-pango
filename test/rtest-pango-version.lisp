(in-package :pango-test)

(def-suite pango-version :in pango-suite)
(in-suite pango-version)

;;;     pango_version

(test version
  (is (= 15012 (pango:version))))

;;;     pango_version_string

(test version-string
  (is (string= "1.50.12" (pango:version-string))))

;;;     pango_version_check

(test version-check
  (is-false (pango:version-check 1 48 0))
  (is (string= "Pango version too old (micro mismatch)"
               (pango:version-check 1 52 0))))

;;; --- 2023-4-29 --------------------------------------------------------------
