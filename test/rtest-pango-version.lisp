(in-package :pango-test)

(def-suite pango-version-suite :in pango-suite)
(in-suite pango-version-suite)

;;;     pango_version

#+crategus
(test pango-version
  (is (= 15603 (pango:version))))

#+windows
(test pango-version
  (is (= 15603 (pango:version))))

;;;     pango_version_string

#+crategus
(test pango-version-string
  (is (string= "1.56.3" (pango:version-string))))

#+windows
(test pango-version-string
  (is (string= "1.56.3" (pango:version-string))))

;;;     pango_version_check

(test pango-version-check
  (is-false (pango:version-check 1 48 0))
  (is (string= "Pango version too old (micro mismatch)"
               (pango:version-check 1 58 0))))

;;; 2025-08-16
