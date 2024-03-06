(in-package :pango-test)

(in-suite pango-suite)

(test pango-test-finished
  (cond (*first-run-pango-test*
         (setf *first-run-pango-test* nil)
         (format t "~%First run of the pango-test suite finished.~%"))
        (t
         (format t "~%Second or more run of the pango-test suite finished.~%"))))

;;; 2024-3-4
