(in-package :pango-test)

(def-suite pango-tab-array :in pango-suite)
(in-suite pango-tab-array)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoTabAlign

(test pango-tab-align
  ;; Check the type
  (is (g:type-is-enum "PangoTabAlign"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoTabAlign")
          (g:gtype (cffi:foreign-funcall "pango_tab_align_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:tab-align
          (glib:symbol-for-gtype "PangoTabAlign")))
  ;; Check the names
  (is (equal '("PANGO_TAB_LEFT" "PANGO_TAB_RIGHT" "PANGO_TAB_CENTER" 
               "PANGO_TAB_DECIMAL")
             (list-enum-item-name "PangoTabAlign")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "PangoTabAlign")))
  ;; Check the nick names
  (is (equal '("left" "right" "center" "decimal")
             (list-enum-item-nick "PangoTabAlign")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoTabAlign"
                                     PANGO-TAB-ALIGN
                                     (:EXPORT T 
                                      :TYPE-INITIALIZER 
                                      "pango_tab_align_get_type")
                                     (:LEFT 0)
                                     (:RIGHT 1)
                                     (:CENTER 2)
                                     (:DECIMAL 3))
             (gobject:get-g-type-definition "PangoTabAlign"))))

;;;     PangoTabArray

(test pango-tab-array-boxed
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoTabArray") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoTabArray")
          (g:gtype (cffi:foreign-funcall "pango_tab_array_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:tab-array
          (glib:symbol-for-gtype "PangoTabArray"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_tab_array_new

(test pango-tab-array-new
  (is (typep (pango:tab-array-new 3 t) 'pango:tab-array)))

;;;     pango_tab_array_new_with_positions

(test pango-tab-array-new-with-positions
  (let ((tabs (pango:tab-array-new-with-positions 3 t
                                                  :left 0
                                                  :decimal 150
                                                  :right 290)))

    (is (equal '(:left 0)
               (multiple-value-list (pango:tab-array-tab tabs 0))))
    (is (equal '(:decimal 150)
               (multiple-value-list (pango:tab-array-tab tabs 1))))
    (is (equal '(:right 290)
               (multiple-value-list (pango:tab-array-tab tabs 2))))
))

;;;     pango_tab_array_copy
;;;     pango_tab_array_free
;;;     pango_tab_array_get_size
;;;     pango_tab_array_resize

;;;     pango_tab_array_get_tab
;;;     pango_tab_array_set_tab

(test pango-tab-array-tab
  (let ((tabs (pango:tab-array-new 3 t)))

    (setf (pango:tab-array-tab tabs 0) '(:left 0))
    (setf (pango:tab-array-tab tabs 1) '(:decimal 150))
    (setf (pango:tab-array-tab tabs 2) '(:right 290))

    (is (equal '(:left 0)
               (multiple-value-list (pango:tab-array-tab tabs 0))))
    (is (equal '(:decimal 150)
               (multiple-value-list (pango:tab-array-tab tabs 1))))
    (is (equal '(:right 290)
               (multiple-value-list (pango:tab-array-tab tabs 2))))
))

;;;     pango_tab_array_get_tabs

;;;     pango_tab_array_get_positions_in_pixels
;;;     pango_tab_array_set_positions_in_pixels            Since 1.50

;;;     pango_tab_array_get_decimal_point                  Since 1.50
;;;     pango_tab_array_set_decimal_point                  Since 1.50
;;;     pango_tab_array_sort                               Since 1.50
;;;     pango_tab_array_from_string                        Since 1.50
;;;     pango_tab_array_to_string                          Since 1.50

;;; --- 2023-10-14 -------------------------------------------------------------
