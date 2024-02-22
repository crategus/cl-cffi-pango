(in-package :pango-test)

(def-suite pango-gravity-suite :in pango-suite)
(in-suite pango-gravity-suite)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoGravity

(test pango-gravity
  ;; Check the type
  (is (g:type-is-enum "PangoGravity"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoGravity")
          (g:gtype (cffi:foreign-funcall "pango_gravity_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:gravity
          (glib:symbol-for-gtype "PangoGravity")))
  ;; Check the names
  (is (equal '("PANGO_GRAVITY_SOUTH" "PANGO_GRAVITY_EAST" "PANGO_GRAVITY_NORTH"
               "PANGO_GRAVITY_WEST" "PANGO_GRAVITY_AUTO")
             (list-enum-item-name "PangoGravity")))
  ;; Check the values
  (is (equal '(0 1 2 3 4)
             (list-enum-item-value "PangoGravity")))
  ;; Check the nick names
  (is (equal '("south" "east" "north" "west" "auto")
             (list-enum-item-nick "PangoGravity")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoGravity"
                             PANGO-GRAVITY
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_gravity_get_type")
                             (:SOUTH 0)
                             (:EAST 1)
                             (:NORTH 2)
                             (:WEST 3)
                             (:AUTO 4))
             (gobject:get-g-type-definition "PangoGravity"))))

;;;     PangoGravityHint

(test pango-gravity-hint
  ;; Check the type
  (is (g:type-is-enum "PangoGravityHint"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoGravityHint")
          (g:gtype (cffi:foreign-funcall "pango_gravity_hint_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:gravity-hint
          (glib:symbol-for-gtype "PangoGravityHint")))
  ;; Check the names
  (is (equal '("PANGO_GRAVITY_HINT_NATURAL" "PANGO_GRAVITY_HINT_STRONG"
               "PANGO_GRAVITY_HINT_LINE")
             (list-enum-item-name "PangoGravityHint")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "PangoGravityHint")))
  ;; Check the nick names
  (is (equal '("natural" "strong" "line")
             (list-enum-item-nick "PangoGravityHint")))
  ;; Check the enum definition
  (is (equal '(GOBJECT:DEFINE-G-ENUM "PangoGravityHint"
                             PANGO-GRAVITY-HINT
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_gravity_hint_get_type")
                             (:NATURAL 0)
                             (:STRONG 1)
                             (:LINE 2))
             (gobject:get-g-type-definition "PangoGravityHint"))))

;;; --- Functions --------------------------------------------------------------

;;;     PANGO_GRAVITY_IS_IMPROPER
;;;     PANGO_GRAVITY_IS_VERTICAL

;;;     pango_gravity_get_for_matrix

(test pango-gravity-for-matrix
  (let ((matrix (pango:matrix-init)))
    (is (eq :south (pango:gravity-for-matrix matrix)))
    (is-false (pango:matrix-rotate matrix 90))
    (is (eq :west (pango:gravity-for-matrix matrix)))
    (is-false (pango:matrix-rotate matrix 90))
    (is (eq :north (pango:gravity-for-matrix matrix)))
    (is-false (pango:matrix-rotate matrix 90))
    (is (eq :east (pango:gravity-for-matrix matrix)))
    (is-false (pango:matrix-rotate matrix 90))
    (is (eq :south (pango:gravity-for-matrix matrix)))))

;;;     pango_gravity_get_for_script

(test pango-gravity-for-script
  (is (eq :south (pango:gravity-for-script :common :auto :strong)))
  (is (eq :south (pango:gravity-for-script :arabic :auto :strong))))

;;;     pango_gravity_get_for_script_and_width

(test pango-gravity-for-script-and-width
  (is (eq :south (pango:gravity-for-script-and-width :common t :auto :strong)))
  (is (eq :south (pango:gravity-for-script-and-width :arabic t :auto :strong))))

;;;     pango_gravity_to_rotation

(test pango-gravity-to-rotation
  (let ((matrix (pango:matrix-init)))
    (is (= 0 (pango:gravity-to-rotation (pango:gravity-for-matrix matrix))))
    (is-false (pango:matrix-rotate matrix 90))
    (is (approx-equal (/ pi 2)
                 (pango:gravity-to-rotation (pango:gravity-for-matrix matrix))))
    (is-false (pango:matrix-rotate matrix 90))
    (is (approx-equal pi
                 (pango:gravity-to-rotation (pango:gravity-for-matrix matrix))))
    (is-false (pango:matrix-rotate matrix 90))
    (is (approx-equal (/ pi -2)
                 (pango:gravity-to-rotation (pango:gravity-for-matrix matrix))))
    (is-false (pango:matrix-rotate matrix 90))
    (is (= 0 (pango:gravity-to-rotation (pango:gravity-for-matrix matrix))))))

;;; 2024-2-22
