(in-package :pango-test)

(def-suite pango-coverage :in pango-suite)
(in-suite pango-coverage)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoCoverageLevel

(test pango-coverage-level
  ;; Check type
  (is (g:type-is-enum "PangoCoverageLevel"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoCoverageLevel")
          (g:gtype (cffi:foreign-funcall "pango_coverage_level_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:coverage-level
          (glib:symbol-for-gtype "PangoCoverageLevel")))
  ;; Check names
  (is (equal '("PANGO_COVERAGE_NONE" "PANGO_COVERAGE_FALLBACK"
               "PANGO_COVERAGE_APPROXIMATE" "PANGO_COVERAGE_EXACT")
             (glib-test:list-enum-item-names "PangoCoverageLevel")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "PangoCoverageLevel")))
  ;; Check nick names
  (is (equal '("none" "fallback" "approximate" "exact")
             (glib-test:list-enum-item-nicks "PangoCoverageLevel")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoCoverageLevel" PANGO:COVERAGE-LEVEL
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "pango_coverage_level_get_type")
                                    (:NONE 0)
                                    (:FALLBACK 1)
                                    (:APPROXIMATE 2)
                                    (:EXACT 3))
             (gobject:get-gtype-definition "PangoCoverageLevel"))))

;;;     PangoCoverage

(test pango-coverage-class
  ;; Check type
  (is (g:type-is-object "PangoCoverage"))
  ;; Check registered name
  (is (eq 'pango:coverage
          (glib:symbol-for-gtype "PangoCoverage")))
  ;; Check type initializer
  (is (eq (g:gtype "PangoCoverage")
          (g:gtype (cffi:foreign-funcall "pango_coverage_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoCoverage")))
  ;; Check children
  #-windows
  (is (equal '("PangoFcCoverage")
             (glib-test:list-children "PangoCoverage")))
  #+windows
  (is (equal '()
             (glib-test:list-children "PangoCoverage")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "PangoCoverage")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "PangoCoverage")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "PangoCoverage")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "PangoCoverage" PANGO:COVERAGE
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "pango_coverage_get_type")
                       NIL)
             (gobject:get-gtype-definition "PangoCoverage"))))

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

;;; 2024-9-18
