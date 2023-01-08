(in-package :pango-test)

(def-suite pango-vertical-text :in pango-suite)
(in-suite pango-vertical-text)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoGravity

(test gravity
  ;; Check the type
  (is (g:type-is-enum "PangoGravity"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoGravity")
          (g:gtype (cffi:foreign-funcall "pango_gravity_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:gravity
          (gobject:symbol-for-gtype "PangoGravity")))
  ;; Check the names
  (is (equal '("PANGO_GRAVITY_SOUTH" "PANGO_GRAVITY_EAST" "PANGO_GRAVITY_NORTH"
               "PANGO_GRAVITY_WEST" "PANGO_GRAVITY_AUTO")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "PangoGravity"))))
  ;; Check the values
  (is (equal '(0 1 2 3 4)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "PangoGravity"))))
  ;; Check the nick names
  (is (equal '("south" "east" "north" "west" "auto")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "PangoGravity"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoGravity"
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

(test gravity-hint
  ;; Check the type
  (is (g:type-is-enum "PangoGravityHint"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoGravityHint")
          (g:gtype (cffi:foreign-funcall "pango_gravity_hint_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:gravity-hint
          (gobject:symbol-for-gtype "PangoGravityHint")))
  ;; Check the names
  (is (equal '("PANGO_GRAVITY_HINT_NATURAL" "PANGO_GRAVITY_HINT_STRONG"
               "PANGO_GRAVITY_HINT_LINE")
             (mapcar #'gobject:enum-item-name
                     (gobject:get-enum-items "PangoGravityHint"))))
  ;; Check the values
  (is (equal '(0 1 2)
             (mapcar #'gobject:enum-item-value
                     (gobject:get-enum-items "PangoGravityHint"))))
  ;; Check the nick names
  (is (equal '("natural" "strong" "line")
             (mapcar #'gobject:enum-item-nick
                     (gobject:get-enum-items "PangoGravityHint"))))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoGravityHint"
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

(test gravity-for-matrix
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

(test gravity-for-script
  (is (eq :south (pango:gravity-for-script :common :auto :strong)))
  (is (eq :south (pango:gravity-for-script :arabic :auto :strong))))

;;;     pango_gravity_get_for_script_and_width

(test gravity-for-script-and-width
  (is (eq :south (pango:gravity-for-script-and-width :common t :auto :strong)))
  (is (eq :south (pango:gravity-for-script-and-width :arabic t :auto :strong))))

;;;     pango_gravity_to_rotation

(test gravity-to-rotation
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

;;; --- 2023-1-3 ---------------------------------------------------------------
