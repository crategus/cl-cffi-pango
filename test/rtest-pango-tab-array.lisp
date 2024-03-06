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
               (multiple-value-list (pango:tab-array-tab tabs 2))))))

;;;     pango_tab_array_copy

(test pango-tab-array-copy
  (let* ((tabs1 (pango:tab-array-from-string "100 decimal:150 right:250"))
         (tabs2 (pango:tab-array-copy tabs1)))
    (is (= 3 (pango:tab-array-size tabs1)))
    (is (= 3 (pango:tab-array-size tabs2)))
    (is (string= (pango:tab-array-to-string tabs1)
                 (pango:tab-array-to-string tabs2)))))

;;;     pango_tab_array_get_size
;;;     pango_tab_array_resize

(test pango-tab-array-size/resize
  (let ((tabs (pango:tab-array-from-string "100 decimal:150 right:250")))
    (is (= 3 (pango:tab-array-size tabs)))
    (is-false (pango:tab-array-resize tabs 6))
    (is (= 6 (pango:tab-array-size tabs)))))

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
               (multiple-value-list (pango:tab-array-tab tabs 2))))))

;;;     pango_tab_array_get_tabs

(test pango-tab-array-tabs
  (let ((tabs (pango:tab-array-from-string "100 decimal:150 right:200")))
    (is (equal '(:left 100)
               (multiple-value-list (pango:tab-array-tab tabs 0))))
    (is (equal '(:decimal 150)
               (multiple-value-list (pango:tab-array-tab tabs 1))))
    (is (equal '(:right 200)
               (multiple-value-list (pango:tab-array-tab tabs 2))))
    (is (equal '((:LEFT 100) (:DECIMAL 150) (:RIGHT 200))
               (pango:tab-array-tabs tabs)))))

;;;     pango_tab_array_get_positions_in_pixels
;;;     pango_tab_array_set_positions_in_pixels            Since 1.50

(test pango-tab-array-positions-in-pixels.1
  (let ((tabs (pango:tab-array-from-string "0px decimal:150px right:290px")))
    (is-true (pango:tab-array-positions-in-pixels tabs))
    (is-false (setf (pango:tab-array-positions-in-pixels tabs) nil))
    (is-false (pango:tab-array-positions-in-pixels tabs))
    (is (string=
"0
decimal:150
right:290"
                 (pango:tab-array-to-string tabs)))))

(test pango-tab-array-positions-in-pixels.2
  (let ((tabs (pango:tab-array-from-string "0 decimal:150 right:290")))
    (is-false (pango:tab-array-positions-in-pixels tabs))
    (is-true (setf (pango:tab-array-positions-in-pixels tabs) t))
    (is-true (pango:tab-array-positions-in-pixels tabs))
    (is (string=
"0px
decimal:150px
right:290px"
                 (pango:tab-array-to-string tabs)))))

;;;     pango_tab_array_get_decimal_point                  Since 1.50
;;;     pango_tab_array_set_decimal_point                  Since 1.50

(test pango-tab-array-decimal-point
  (let ((tabs (pango:tab-array-from-string "0px decimal:150px right:290px")))
    (is (eq #\Nul (pango:tab-array-decimal-point tabs 1)))
    (is (eq #\. (setf (pango:tab-array-decimal-point tabs 1) #\.)))
    (is (eq #\. (pango:tab-array-decimal-point tabs 1)))))

;;;     pango_tab_array_sort                               Since 1.50

(test pango-tab-array-from-string
  (let ((tabs (pango:tab-array-new-with-positions 3 t
                                                  :right 290
                                                  :decimal 150
                                                  :left 0)))
    (is (= 3 (pango:tab-array-size tabs)))
    ;; TODO: This is not a test for sorting, the array is already sorted!?
    (is-false (pango:tab-array-sort tabs))
    (is (string=
"0px
decimal:150px
right:290px"
                 (pango:tab-array-to-string tabs)))))

;;;     pango_tab_array_from_string                        Since 1.50

(test pango-tab-array-from-string
  (let ((tabs (pango:tab-array-from-string "0px decimal:150px right:290px")))
    (is (= 3 (pango:tab-array-size tabs)))
    (is (string=
"0px
decimal:150px
right:290px"
                 (pango:tab-array-to-string tabs)))))

;;;     pango_tab_array_to_string                          Since 1.50

(test pango-tab-array-to-string
  (let ((tabs (pango:tab-array-new-with-positions 3 t
                                                  :left 0
                                                  :decimal 150
                                                  :right 290)))
    (is (string=
"0px
decimal:150px
right:290px"
                 (pango:tab-array-to-string tabs)))))
;;; 2024-2-24
