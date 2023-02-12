(in-package :pango-test)

(def-suite pango-layout :in pango-suite)
(in-suite pango-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoWrapMode

(test wrap-mode
  ;; Check the type
  (is (g:type-is-enum "PangoWrapMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoWrapMode")
          (g:gtype (cffi:foreign-funcall "pango_wrap_mode_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:wrap-mode
          (gobject:symbol-for-gtype "PangoWrapMode")))
  ;; Check the names
  (is (equal '("PANGO_WRAP_WORD" "PANGO_WRAP_CHAR" "PANGO_WRAP_WORD_CHAR")
             (list-enum-item-name "PangoWrapMode")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "PangoWrapMode")))
  ;; Check the nick names
  (is (equal '("word" "char" "word-char")
             (list-enum-item-nick "PangoWrapMode")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoWrapMode"
                             PANGO-WRAP-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_wrap_mode_get_type")
                             (:WORD 0)
                             (:CHAR 1)
                             (:WORD-CHAR 2))
             (gobject:get-g-type-definition "PangoWrapMode"))))

;;;     PangoEllipsizeMode

(test ellipsize-mode
  ;; Check the type
  (is (g:type-is-enum "PangoEllipsizeMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoEllipsizeMode")
          (g:gtype (cffi:foreign-funcall "pango_ellipsize_mode_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:ellipsize-mode
          (gobject:symbol-for-gtype "PangoEllipsizeMode")))
  ;; Check the names
  (is (equal '("PANGO_ELLIPSIZE_NONE" "PANGO_ELLIPSIZE_START"
               "PANGO_ELLIPSIZE_MIDDLE" "PANGO_ELLIPSIZE_END")
             (list-enum-item-name "PangoEllipsizeMode")))
  ;; Check the values
  (is (equal '(0 1 2 3)
             (list-enum-item-value "PangoEllipsizeMode")))
  ;; Check the nick names
  (is (equal '("none" "start" "middle" "end")
             (list-enum-item-nick "PangoEllipsizeMode")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoEllipsizeMode"
                             PANGO-ELLIPSIZE-MODE
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_ellipsize_mode_get_type")
                             (:NONE 0)
                             (:START 1)
                             (:MIDDLE 2)
                             (:END 3))
             (gobject:get-g-type-definition "PangoEllipsizeMode"))))

;;;     PangoAlignment

(test alignment
  ;; Check the type
  (is (g:type-is-enum "PangoAlignment"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoAlignment")
          (g:gtype (cffi:foreign-funcall "pango_alignment_get_type" :size))))
  ;; Check the registered name
  (is (eq 'pango:alignment
          (gobject:symbol-for-gtype "PangoAlignment")))
  ;; Check the names
  (is (equal '("PANGO_ALIGN_LEFT" "PANGO_ALIGN_CENTER" "PANGO_ALIGN_RIGHT")
             (list-enum-item-name "PangoAlignment")))
  ;; Check the values
  (is (equal '(0 1 2)
             (list-enum-item-value "PangoAlignment")))
  ;; Check the nick names
  (is (equal '("left" "center" "right")
             (list-enum-item-nick "PangoAlignment")))
  ;; Check the enum definition
  (is (equal '(DEFINE-G-ENUM "PangoAlignment"
                             PANGO-ALIGNMENT
                             (:EXPORT T
                              :TYPE-INITIALIZER "pango_alignment_get_type")
                             (:LEFT 0)
                             (:CENTER 1)
                             (:RIGHT 2))
             (gobject:get-g-type-definition "PangoAlignment"))))

;;;     PangoLayoutRun

;;;     PangoLayoutLine

(test layout-line-boxed
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoLayoutLine") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoLayoutLine")
          (g:gtype (cffi:foreign-funcall "pango_layout_line_get_type" :size)))))

;;;     PangoLayoutIter

(test layout-iter-boxed
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoLayoutIter") +g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoLayoutIter")
          (g:gtype (cffi:foreign-funcall "pango_layout_iter_get_type" :size)))))

;;;     PangoLayout

(test pango-layout-class
  ;; Type check
  (is (g:type-is-object "PangoLayout"))
  ;; Check the registered name
  (is (eq 'pango:layout
          (gobject:symbol-for-gtype "PangoLayout")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoLayout")
          (g:gtype (cffi:foreign-funcall "pango_layout_get_type" :size))))
  ;; Check the parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoLayout")))
  ;; Check the children
  (is (equal '()
             (list-children "PangoLayout")))
  ;; Check the interfaces
  (is (equal '()
             (list-interfaces "PangoLayout")))
  ;; Check the class properties
  (is (equal '()
             (list-properties "PangoLayout")))
  ;; Check the signals
  (is (equal '()
             (list-signals "PangoLayout")))
  ;; Check the class definition
  (is (equal '(DEFINE-G-OBJECT-CLASS "PangoLayout" PANGO-LAYOUT
                       (:SUPERCLASS G-OBJECT :EXPORT T :INTERFACES NIL
                        :TYPE-INITIALIZER "pango_layout_get_type")
                       NIL)
             (gobject:get-g-type-definition "PangoLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango-layout-new

(test layout-new
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let ((context (pango:cairo-create-context cr)))
      (is (typep context 'pango:context))
      (is (typep (pango:layout-new context) 'pango:layout)))))

;;;     pango_layout_copy

(test layout-copy
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (typep (pango:layout-copy layout) 'pango:layout)))))

;;;     pango-layout-context

(test layout-context
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
       (eq context (pango:layout-context layout)))))

;;;     pango-layout-context-changed
;;;     pango-layout-serial

(test layout-serial/context-changed
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
       (is (= 1 (pango:layout-serial layout)))
       (is-false (pango:layout-context-changed layout))
       (is (= 2 (pango:layout-serial layout))))))

;;;     pango-layout-text

(test layout-text
  (let ((layout (make-instance 'pango:layout)))
    (is (string= "" (pango:layout-text layout)))
    (is (string= "text" (setf (pango:layout-text layout) "text")))
    (is (string= "text" (pango:layout-text layout)))))

;;;     pango-layout-character-count

(test layout-character-count
  (let ((layout (make-instance 'pango:layout)))
    (is (= 0  (pango:layout-character-count layout)))
    (is (string= "text" (setf (pango:layout-text layout) "text")))
    (is (= 4 (pango:layout-character-count layout)))))

;;;     pango_layout_set_markup
;;;     pango_layout_set_markup_with_accel

(test layout-set-markup
  (let ((layout (make-instance 'pango:layout)))
    (is-false (pango:layout-set-markup layout "<b>Text</b>"))
    (is (string= "Text" (pango:layout-text layout)))
    (is (eq #\T (pango:layout-set-markup-with-accel layout
                                                    "<b>_Text neu</b>" #\_)))
    (is (string= "Text neu" (pango:layout-text layout)))))

;;;     pango_layout_set_attributes
;;;     pango_layout_get_attributes

(test layout-attributes
  (let ((layout (make-instance 'pango:layout)))
    (is-false (pango:layout-set-markup layout "<b>T<small>e</small>xt</b>"))
    (is (string= "Text" (pango:layout-text layout)))
    (is (typep (pango:layout-attributes layout) 'pango:attr-list))

    (is (every (lambda (x) (typep x 'pango:attribute))
               (pango:attr-list-attributes (pango:layout-attributes layout))))

    (is (equal '(:weight :scale)
               (mapcar #'pango:attribute-type
                       (pango:attr-list-attributes
                           (pango:layout-attributes layout)))))))

;;;     pango_layout_set_font_description
;;;     pango_layout_get_font_description

(test layout-font-description
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is-false (pango:layout-font-description layout))
      (is (typep (setf (pango:layout-font-description layout)
                        (pango:font-description-new))
                 'pango:font-description))
      (is (typep (pango:layout-font-description layout)
                 'pango:font-description)))))

;;;     pango_layout_set_width
;;;     pango_layout_get_width
;;;     pango_layout_set_height
;;;     pango_layout_get_height

(test layout-width/height
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some Text"
                   (setf (pango:layout-text layout) "Some Text")))
      (is (= -1 (pango:layout-width layout)))
      (is (= -1 (pango:layout-height layout)))
      (is (= 10 (setf (pango:layout-width layout) 10)))
      (is (= 10 (pango:layout-width layout)))
      (is (= 20 (setf (pango:layout-height layout) 20)))
      (is (= 20 (pango:layout-height layout))))))

;;;     pango_layout_set_wrap
;;;     pango_layout_get_wrap
;;;     pango_layout_is_wrapped
;;;     pango_layout_set_ellipsize
;;;     pango_layout_get_ellipsize
;;;     pango_layout_is_ellipsized
;;;     pango_layout_set_indent
;;;     pango_layout_get_indent
;;;     pango_layout_get_spacing
;;;     pango_layout_set_spacing
;;;     pango_layout_set_line_spacing
;;;     pango_layout_get_line_spacing
;;;     pango_layout_set_justify
;;;     pango_layout_get_justify
;;;     pango_layout_set_auto_dir
;;;     pango_layout_get_auto_dir
;;;     pango_layout_get_direction
;;;     pango_layout_set_alignment
;;;     pango_layout_get_alignment
;;;     pango_layout_set_tabs
;;;     pango_layout_get_tabs
;;;     pango_layout_set_single_paragraph_mode
;;;     pango_layout_get_single_paragraph_mode
;;;     pango_layout_get_unknown_glyphs_count

;;;     pango_layout_get_log_attrs
;;;     pango_layout_get_log_attrs_readonly

;; FIXME: Do we need pango:log-attrs instances?! Is it sufficent to pass
;; a pointer to the array of pango:log-attrs? Do more work later.

#+nil
(test layout-log-attrs
  (let* ((widget (make-instance 'gtk-label))
         (context (gtk-widget-pango-context widget))
         (layout (pango:layout-new context)))
    (is (string= "some text"
                 (setf (pango:layout-text layout) "some text")))

    (is-false (pango:layout-log-attrs layout))
))

#+nil
(test layout-log-attrs
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is-false (pango:layout-set-markup layout "<b>T<small>e</small>xt</b>"))

      (cffi:with-foreign-objects ((attrs-ptr :pointer) (n-attrs :int))
        (is-false (pango::%layout-log-attrs layout attrs-ptr n-attrs))
        (is-false attrs-ptr)
        (is-false (cffi:mem-ref n-attrs :int))
))))

#+nil
(test layout-log-attrs.1
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is-false (pango:layout-set-markup layout "<b>T<small>e</small>xt</b>"))

      (cffi:with-foreign-object (n-attrs :int)
        (let ((attrs-ptr (pango::%layout-log-attrs-readonly layout n-attrs)))
          (is-false attrs-ptr)
          (is-false (cffi:mem-ref n-attrs :int))
          (is-false
            (loop for i from 0 below (cffi:mem-ref n-attrs :int)
                  ;; This collects a list of pointers
                  collect (cffi:mem-ref attrs-ptr :pointer i)
                  finally (glib:free attrs-ptr)))
)))))

#+nil
(test layout-log-attrs.2
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is-false (pango:layout-set-markup layout "<b>T<small>e</small>xt</b>"))

      (is-false (pango:layout-log-attrs-readonly layout))
)))

;;;     pango_layout_index_to_pos

(test layout-index-to-pos
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 0)
        (is (=     0 x))
        (is (=     0 y))
        (is (= 11264 width))
        (is (= 19456 height)))

      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 1)
        (is (= 11264 x))
        (is (=     0 y))
        (is (= 10240 width))
        (is (= 19456 height)))

      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 2)
        (is (= 21504 x))
        (is (=     0 y))
        (is (= 15360 width))
        (is (= 19456 height)))
)))

;;;     pango_layout_index_to_line_x

(test layout-index-to-line-x
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '(0 0)
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 0 nil))))
      (is (equal '(0 11264)
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 1 nil))))
      (is (equal '(0 21504)
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 2 nil)))))))

;;;     pango_layout_xy_to_index

(test layout-xy-to-index
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '(0 0)
                 (multiple-value-list (pango:layout-xy-to-index layout 0 0))))
      (is (equal '(1 0)
                 (multiple-value-list (pango:layout-xy-to-index layout 11264 0))))
      (is (equal '(2 0)
                 (multiple-value-list (pango:layout-xy-to-index layout 21504 0))))
)))

;;;     pango_layout_get_cursor_pos

(test layout-cursor-pos
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '((0 0 0 19456) (0 0 0 19456))
                 (multiple-value-list (pango:layout-cursor-pos layout 0))))
      (is (equal '((11264 0 0 19456) (11264 0 0 19456))
                 (multiple-value-list (pango:layout-cursor-pos layout 1))))
      (is (equal '((21504 0 0 19456) (21504 0 0 19456))
                 (multiple-value-list (pango:layout-cursor-pos layout 2))))
)))

;;;     pango_layout_move_cursor_visually

(test layout-move-cursor-visually
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '(1 0)
                 (multiple-value-list
                     (pango:layout-move-cursor-visually layout t 0 0 1))))
      (is (equal '(2 0)
                 (multiple-value-list
                     (pango:layout-move-cursor-visually layout t 1 0 1))))
      (is (equal '(3 0)
                 (multiple-value-list
                     (pango:layout-move-cursor-visually layout t 2 0 1))))
)))

;;;     pango_layout_get_extents
;;;     pango_layout_get_pixel_extents

(test layout-extents/pixel-extents
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '((1024 3072 81920 12288) (0 0 81920 19456))
                 (multiple-value-list (pango:layout-extents layout))))
      (is (equal '((1 3 80 12) (0 0 80 19))
                 (multiple-value-list (pango:layout-pixel-extents layout))))
)))

;;;     pango_layout_get_size
;;;     pango_layout_get_pixel_size

(test layout-size/pixel-size
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '(81920 19456)
                 (multiple-value-list (pango:layout-size layout))))
      (is (equal '(80 19)
                 (multiple-value-list (pango:layout-pixel-size layout))))
)))

;;;     pango_layout_get_baseline
;;;     pango_layout_get_line_count

(test layout-baseline/line-count
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (= 15360 (pango:layout-baseline layout)))
      (is (= 30 (pango:layout-line-count layout)))
)))

;;;     pango_layout_get_line

(test layout-line
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (= 30 (pango:layout-line-count layout)))
      (loop for count from 0 below (pango:layout-line-count layout)
            do (is (typep (pango:layout-line layout count) 'pango:layout-line)))
      ;; A second run
      (is (= 30 (pango:layout-line-count layout)))
      (loop for count from 0 below (pango:layout-line-count layout)
            do (is (typep (pango:layout-line layout count) 'pango:layout-line)))
)))

;;;     pango_layout_get_line_readonly

(test layout-line-readonly
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (= 30 (pango:layout-line-count layout)))
      (loop for count from 0 below (pango:layout-line-count layout)
            do (is (typep (pango:layout-line-readonly layout count)
                          'pango:layout-line)))
      ;; A second run
      (is (= 30 (pango:layout-line-count layout)))
      (loop for count from 0 below (pango:layout-line-count layout)
            do (is (typep (pango:layout-line-readonly layout count)
                          'pango:layout-line)))
)))

;;;     pango_layout_get_lines

;; TODO: Crashes in the second run
;; What is wrong???

(test layout-lines
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (= 30 (pango:layout-line-count layout)))
      (is (every (lambda (x) (typep x 'pango:layout-line))
                 (pango:layout-lines layout)))
      ;; A second run
      (is (= 30 (length (pango:layout-lines layout))))
      (is (every (lambda (x) (typep x 'pango:layout-line))
                 (pango:layout-lines layout)))
)))

;;;     pango_layout_get_lines_readonly

(test layout-lines-readonly
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (= 30 (pango:layout-line-count layout)))
      (is (every (lambda (x) (typep x 'pango:layout-line))
                 (pango:layout-lines-readonly layout)))
      ;; A second run
      (is (= 30 (length (pango:layout-lines-readonly layout))))
      (is (every (lambda (x) (typep x 'pango:layout-line))
                 (pango:layout-lines-readonly layout)))
)))

;;;     pango_layout_get_iter

(test layout-iter
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (typep (pango:layout-iter layout) 'pango:layout-iter))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (typep (pango:layout-iter layout) 'pango:layout-iter))
)))

;;;     pango_layout_iter_copy

(test layout-iter-copy
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (typep (pango:layout-iter-copy iter) 'pango:layout-iter))
)))

;;;     pango_layout_iter_next_run

(test layout-iter-next-run
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (= 0 (pango:layout-iter-index iter)))
      (is-true (pango:layout-iter-next-run iter))
      (is (= 78 (pango:layout-iter-index iter)))
)))

;;;     pango_layout_iter_next_char
;;;     pango_layout_iter_get_index

(test layout-iter-next-char
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (= 0 (pango:layout-iter-index iter)))
      (is-true (pango:layout-iter-next-char iter))
      (is (= 1 (pango:layout-iter-index iter)))
      (is-true (pango:layout-iter-next-char iter))
      (is (= 2 (pango:layout-iter-index iter)))
      (is-true (pango:layout-iter-next-char iter))
      (is (= 3 (pango:layout-iter-index iter)))
)))

;;;     pango_layout_iter_next_cluster

(test layout-iter-next-cluster
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (= 0 (pango:layout-iter-index iter)))

      (is-true (pango:layout-iter-next-cluster iter))
      (is (= 1 (pango:layout-iter-index iter)))
      (is-true (pango:layout-iter-next-cluster iter))
      (is (= 2 (pango:layout-iter-index iter)))
)))

;;;     pango_layout_iter_next_line
;;;     pango_layout_iter_at_last_line
;;;     pango_layout_iter_get_baseline

(test layout-iter-line/next-line/at-last-line
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (= 0 (pango:layout-iter-index iter)))
      (is (= 15360 (pango:layout-iter-baseline iter)))
      (is-true (pango:layout-iter-next-line iter))
      (is (= 79 (pango:layout-iter-index iter)))
      (is-false (pango:layout-iter-at-last-line iter))
      (is (= 34816 (pango:layout-iter-baseline iter)))
)))

;;;     pango_layout_iter_get_run
;;;     pango_layout_iter_get_run_readonly

(test layout-iter-run
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (typep (pango:layout-iter-run iter) 'pango:glyph-item))
      (is (typep (pango:layout-iter-run-readonly iter) 'pango:glyph-item))
)))

;;;     pango_layout_iter_get_line
;;;     pango_layout_iter_get_line_readonly

(test layout-iter-line
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (typep (pango:layout-iter-line iter) 'pango:layout-line))
      (is (typep (pango:layout-iter-line-readonly iter) 'pango:layout-line))
)))

;;;     pango_layout_iter_get_layout

(test layout-iter-line
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (eq layout (pango:layout-iter-layout iter)))
)))

;;;     pango_layout_iter_get_char_extents
;;;     pango_layout_iter_get_cluster_extents
;;;     pango_layout_iter_get_run_extents

(test layout-iter-char/cluster/run-extents
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (equal '(0 0 15360 19456)
                 (multiple-value-list (pango:layout-iter-char-extents iter))))
      (is (equal '((0 3072 17408 12288) (0 0 15360 19456))
                 (multiple-value-list (pango:layout-iter-cluster-extents iter))))
      (is (equal '((0 2048 659456 17408) (0 0 659456 19456))
                 (multiple-value-list (pango:layout-iter-run-extents iter))))
)))

;;;     pango_layout_iter_get_line_yrange

(test layout-iter-line-yrange
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (equal '(0 19456)
                 (multiple-value-list (pango:layout-iter-line-yrange iter))))
)))

;;;     pango_layout_iter_get_line_extents
;;;     pango_layout_iter_get_layout_extents

(test layout-iter-line/layout-extents
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (equal '((0 2048 659456 17408) (0 0 659456 19456))
                 (multiple-value-list (pango:layout-iter-line-extents iter))))
      (is (equal '((0 2048 659456 17408) (0 0 659456 19456))
                 (multiple-value-list (pango:layout-iter-layout-extents iter))))
)))

;;;     pango_layout_line_get_extents
;;;     pango_layout_line_get_pixel_extents

;; TODO: All functions for pango:layout-line do not work. We get fatal errors
;; with the following tests.

#+nil
(test layout-line-extents/pixel-extents
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (line nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))

      (is (typep (setf line (pango:layout-line layout 0)) 'pango:layout-line))

      (is (equal '((0 2048 659456 17408) (0 0 659456 19456))
                 (multiple-value-list (pango:layout-line-extents line))))

;      (is (equal '((0 2048 659456 17408) (0 0 659456 19456))
;                 (multiple-value-list (pango:layout-line-pixel-extents line))))

)))

;;;     pango_layout_line_index_to_x
;;;     pango_layout_line_x_to_index

#+nil
(test layout-line-index-to-x/x-to-index
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (line nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))

      (is (typep (setf line (pango:layout-line layout 1)) 'pango:layout-line))
      (is-false (pango:layout-line-index-to-x line 5 nil))
)))

;;;     pango_layout_line_get_x_ranges

;;;     pango_layout_line_get_height

#+nil
(test layout-line-height
  (with-cairo-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (line nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))

      (trace pango:layout-line)
      (trace cffi:convert-from-foreign)
      (trace cffi:translate-from-foreign)
      (trace cffi:translate-to-foreign)
      (is (typep (setf line (pango:layout-line layout 1)) 'pango:layout-line))
      (is (typep line 'pango:layout-line))
      (is-false (pango:layout-line-height line))
      (untrace pango:layout-line)
      (untrace cffi:convert-from-foreign)
      (untrace cffi:translate-from-foreign)
      (untrace cffi:translate-to-foreign)
)))

;;; --- 2023-2-11 --------------------------------------------------------------
