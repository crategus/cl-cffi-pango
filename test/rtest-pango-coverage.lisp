(in-package :pango-test)

(def-suite pango-coverage :in pango-suite)
(in-suite pango-coverage)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoCoverageLevel

(test pango-coverage-level
  ;; Check the type
  (is (g:type-is-enum "PangoCoverageLevel"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoCoverageLevel")
          (g:gtype (cffi:foreign-funcall "pango_coverage_level_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:coverage-level
          (glib:symbol-for-gtype "PangoCoverageLevel")))
  ;; Check the names
  (is (equal '("PANGO_COVERAGE_NONE" "PANGO_COVERAGE_FALLBACK"
               "PANGO_COVERAGE_APPROXIMATE" "PANGO_COVERAGE_EXACT")
             (list-enum-item-name "PangoCoverageLevel")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "PangoCoverageLevel")))
  ;; Check the nick names
  (is (equal '("none" "fallback" "approximate" "exact")
             (list-enum-item-nick "PangoCoverageLevel")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoCoverageLevel" PANGO-COVERAGE-LEVEL
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "pango_coverage_level_get_type")
                                     (:NONE 0)
                                     (:FALLBACK 1)
                                     (:APPROXIMATE 2)
                                     (:EXACT 3))
             (gobject:get-g-type-definition "PangoCoverageLevel"))))

;;;     PangoCoverage

(test pango-coverage-class
  ;; Type check
  (is (g:type-is-object "PangoCoverage"))
  ;; Check the registered name
  (is (eq 'pango:coverage
          (glib:symbol-for-gtype "PangoCoverage")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoCoverage")
          (g:gtype (cffi:foreign-funcall "pango_coverage_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoCoverage")))
  ;; Check the children
  #-windows
  (is (equal '("PangoFcCoverage")
             (list-children "PangoCoverage")))
  #+windows
  (is (equal '()
             (list-children "PangoCoverage")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "PangoCoverage")))
  ;; Check the properties
  (is (equal '()
             (list-properties "PangoCoverage")))
  ;; Check the signals
  (is (equal '()
             (list-signals "PangoCoverage")))
  ;; Check the class definition
  (is (equal '(GOBJECT:DEFINE-G-OBJECT-CLASS "PangoCoverage" PANGO-COVERAGE
                               (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                                :TYPE-INITIALIZER "pango_coverage_get_type")
                               NIL)
             (gobject:get-g-type-definition "PangoCoverage"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_coverage_new

(test pango-coverage-new
  (is (typep (pango:coverage-new) 'pango:coverage)))

;;;     pango_coverage_ref                                 Deprecated 1.52
;;;     pango_coverage_unref                               Deprecated 1.52
;;;     pango_coverage_copy                                not needed

;;;     pango_coverage_get
;;;     pango_coverage_set

(test pango-coverage-get
  (let ((coverage (pango:coverage-new)))
    (is (eq :none (pango:coverage-get coverage 0)))
    (is-false (pango:coverage-set coverage 0 :exact))
    (is (eq :exact (pango:coverage-get coverage 0)))))

;;;     pango_coverage_max                                 Deprecated 1.44
;;;     pango_coverage_to_bytes                            Deprecated 1.44
;;;     pango_coverage_from_bytes                          Deprecated 1.44

;;; 2024-2-24
