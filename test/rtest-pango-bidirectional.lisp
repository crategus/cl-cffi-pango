(in-package :pango-test)

(def-suite pango-bidirectional :in pango-suite)
(in-suite pango-bidirectional)

;;; --- Types and Value --------------------------------------------------------

;;;     PangoDirection

(test pango-direction
  ;; Check type
  (is (g:type-is-enum "PangoDirection"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoDirection")
          (g:gtype (cffi:foreign-funcall "pango_direction_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:direction
          (glib:symbol-for-gtype "PangoDirection")))
  ;; Check names
  (is (equal '("PANGO_DIRECTION_LTR" "PANGO_DIRECTION_RTL"
               "PANGO_DIRECTION_TTB_LTR" "PANGO_DIRECTION_TTB_RTL"
               "PANGO_DIRECTION_WEAK_LTR" "PANGO_DIRECTION_WEAK_RTL"
               "PANGO_DIRECTION_NEUTRAL")
             (glib-test:list-enum-item-names "PangoDirection")))
  ;; Check values
  (is (equal '(0 1 2 3 4 5 6)
             (glib-test:list-enum-item-values "PangoDirection")))
  ;; Check nick names
  (is (equal '("ltr" "rtl" "ttb-ltr" "ttb-rtl" "weak-ltr" "weak-rtl" "neutral")
             (glib-test:list-enum-item-nicks "PangoDirection")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoDirection" PANGO:DIRECTION
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "pango_direction_get_type")
                                    (:LTR 0)
                                    (:RTL 1)
                                    (:TTB-LTR 2)
                                    (:TTB-RTL 3)
                                    (:WEAK-LTR 4)
                                    (:WEAK-RTL 5)
                                    (:NEUTRAL 6))
             (gobject:get-gtype-definition "PangoDirection"))))

;;;     PangoBidiType                                      Deprecated

;;; --- Function ---------------------------------------------------------------

;;;     pango_unichar_direction

(test pango-unichar-direction
  (is (eq :ltr (pango:unichar-direction #\a)))
  (is (eq :neutral (pango:unichar-direction #\1)))
  (is (eq :neutral (pango:unichar-direction #\?))))

;;;     pango_find_base_dir

(test pango-find-base-dir
  (is (eq :ltr (pango:find-base-dir "text")))
  (is (eq :neutral (pango:find-base-dir "1234"))))

;;;     pango_get_mirror_char                              Deprecated
;;;     pango_bidi_type_for_unichar                        Deprecated

;;; 2024-9-18
