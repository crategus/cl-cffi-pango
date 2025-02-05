(in-package :pango-test)

(def-suite pango-layout :in pango-suite)
(in-suite pango-layout)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoWrapMode

(test pango-wrap-mode
  ;; Check type
  (is (g:type-is-enum "PangoWrapMode"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoWrapMode")
          (g:gtype (cffi:foreign-funcall "pango_wrap_mode_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:wrap-mode
          (glib:symbol-for-gtype "PangoWrapMode")))
  ;; Check names
  (is (equal '("PANGO_WRAP_WORD" "PANGO_WRAP_CHAR" "PANGO_WRAP_WORD_CHAR")
             (glib-test:list-enum-item-names "PangoWrapMode")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "PangoWrapMode")))
  ;; Check nick names
  (is (equal '("word" "char" "word-char")
             (glib-test:list-enum-item-nicks "PangoWrapMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoWrapMode" PANGO:WRAP-MODE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "pango_wrap_mode_get_type")
                                    (:WORD 0)
                                    (:CHAR 1)
                                    (:WORD-CHAR 2))
             (gobject:get-gtype-definition "PangoWrapMode"))))

;;;     PangoEllipsizeMode

(test pango-ellipsize-mode
  ;; Check type
  (is (g:type-is-enum "PangoEllipsizeMode"))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoEllipsizeMode")
          (g:gtype (cffi:foreign-funcall "pango_ellipsize_mode_get_type"
                                         :size))))
  ;; Check registered name
  (is (eq 'pango:ellipsize-mode
          (glib:symbol-for-gtype "PangoEllipsizeMode")))
  ;; Check names
  (is (equal '("PANGO_ELLIPSIZE_NONE" "PANGO_ELLIPSIZE_START"
               "PANGO_ELLIPSIZE_MIDDLE" "PANGO_ELLIPSIZE_END")
             (glib-test:list-enum-item-names "PangoEllipsizeMode")))
  ;; Check values
  (is (equal '(0 1 2 3)
             (glib-test:list-enum-item-values "PangoEllipsizeMode")))
  ;; Check nick names
  (is (equal '("none" "start" "middle" "end")
             (glib-test:list-enum-item-nicks "PangoEllipsizeMode")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoEllipsizeMode" PANGO:ELLIPSIZE-MODE
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "pango_ellipsize_mode_get_type")
                                    (:NONE 0)
                                    (:START 1)
                                    (:MIDDLE 2)
                                    (:END 3))
             (gobject:get-gtype-definition "PangoEllipsizeMode"))))

;;;     PangoAlignment

(test pango-alignment
  ;; Check type
  (is (g:type-is-enum "PangoAlignment"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoAlignment")
          (g:gtype (cffi:foreign-funcall "pango_alignment_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:alignment
          (glib:symbol-for-gtype "PangoAlignment")))
  ;; Check names
  (is (equal '("PANGO_ALIGN_LEFT" "PANGO_ALIGN_CENTER" "PANGO_ALIGN_RIGHT")
             (glib-test:list-enum-item-names "PangoAlignment")))
  ;; Check values
  (is (equal '(0 1 2)
             (glib-test:list-enum-item-values "PangoAlignment")))
  ;; Check nick names
  (is (equal '("left" "center" "right")
             (glib-test:list-enum-item-nicks "PangoAlignment")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoAlignment" PANGO:ALIGNMENT
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "pango_alignment_get_type")
                                    (:LEFT 0)
                                    (:CENTER 1)
                                    (:RIGHT 2))
             (gobject:get-gtype-definition "PangoAlignment"))))

;;;     PangoLayoutRun

;;;     PangoLayoutLine

(test pango-layout-line-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoLayoutLine"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoLayoutLine")
          (g:gtype (cffi:foreign-funcall "pango_layout_line_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:layout-line
          (glib:symbol-for-gtype "PangoLayoutLine"))))

;;;     PangoLayoutIter

(test pango-layout-iter-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoLayoutIter"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoLayoutIter")
          (g:gtype (cffi:foreign-funcall "pango_layout_iter_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:layout-iter
          (glib:symbol-for-gtype "PangoLayoutIter"))))

;;;     PangoLayout

(test pango-layout-class
  ;; Check type
  (is (g:type-is-object "PangoLayout"))
  ;; Check registered name
  (is (eq 'pango:layout
          (glib:symbol-for-gtype "PangoLayout")))
  ;; Check type initializer
  (is (eq (g:gtype "PangoLayout")
          (g:gtype (cffi:foreign-funcall "pango_layout_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoLayout")))
  ;; Check children
  (is (equal '()
             (glib-test:list-children "PangoLayout")))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "PangoLayout")))
  ;; Check class properties
  (is (equal '()
             (glib-test:list-properties "PangoLayout")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "PangoLayout")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "PangoLayout" PANGO:LAYOUT
                       (:SUPERCLASS GOBJECT:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "pango_layout_get_type")
                       NIL)
             (gobject:get-gtype-definition "PangoLayout"))))

;;; --- Functions --------------------------------------------------------------

;;;     pango-layout-new

(test pango-layout-new
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let ((context (pango:cairo-create-context cr)))
      (is (typep context 'pango:context))
      (is (typep (pango:layout-new context) 'pango:layout)))))

;;;     pango_layout_copy

(test pango-layout-copy
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (typep (pango:layout-copy layout) 'pango:layout)))))

;;;     pango-layout-context

(test pango-layout-context
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
       (eq context (pango:layout-context layout)))))

;;;     pango-layout-context-changed
;;;     pango-layout-serial

(test pango-layout-serial/context-changed
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
       (is (= 1 (pango:layout-serial layout)))
       (is-false (pango:layout-context-changed layout))
       (is (= 2 (pango:layout-serial layout))))))

;;;     pango-layout-text

(test pango-layout-text
  (let ((layout (make-instance 'pango:layout)))
    (is (string= "" (pango:layout-text layout)))
    (is (string= "text" (setf (pango:layout-text layout) "text")))
    (is (string= "text" (pango:layout-text layout)))))

;;;     pango-layout-character-count

(test pango-layout-character-count
  (let ((layout (make-instance 'pango:layout)))
    (is (= 0  (pango:layout-character-count layout)))
    (is (string= "text" (setf (pango:layout-text layout) "text")))
    (is (= 4 (pango:layout-character-count layout)))))

;;;     pango_layout_set_markup
;;;     pango_layout_set_markup_with_accel

(test pango-layout-set-markup
  (let ((layout (make-instance 'pango:layout)))
    (is-false (pango:layout-set-markup layout "<b>Text</b>"))
    (is (string= "Text" (pango:layout-text layout)))
    (is (eq #\T (pango:layout-set-markup-with-accel layout
                                                    "<b>_Text neu</b>" #\_)))
    (is (string= "Text neu" (pango:layout-text layout)))))

;;;     pango_layout_set_attributes
;;;     pango_layout_get_attributes

(test pango-layout-attributes
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

(test pango-layout-font-description
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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

(test pango-layout-width/height
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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

(test pango-layout-line-spacing
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (= 0.0 (pango:layout-line-spacing layout)))
      (is (= 1.5 (setf (pango:layout-line-spacing layout) 3/2)))
      (is (= 1.5 (pango:layout-line-spacing layout))))))

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

;;;     pango_layout_get_log_attrs                         not exported
;;;     pango_layout_get_log_attrs_readonly                not exported

;;;     pango_layout_index_to_pos

(test pango-layout-index-to-pos.1
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 0)
        (is (integerp x))
        (is (integerp y))
        (is (integerp width))
        (is (integerp height)))

      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 1)
        (is (integerp x))
        (is (integerp y))
        (is (integerp width))
        (is (integerp height)))

      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 2)
        (is (integerp x))
        (is (integerp y))
        (is (integerp width))
        (is (integerp height))))))

#+crategus
(test pango-layout-index-to-pos.2
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 0)
        (is (=     0 x))
        (is (=     0 y))
        (is (=  9216 width))
        (is (= 23552 height)))

      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 1)
        (is (=  9216 x))
        (is (=     0 y))
        (is (=  9216 width))
        (is (= 23552 height)))

      (multiple-value-bind (x y width height)
          (pango:layout-index-to-pos layout 2)
        (is (= 18432 x))
        (is (=     0 y))
        (is (= 15360 width))
        (is (= 23552 height))))))

;;;     pango_layout_index_to_line_x

(test pango-layout-index-to-line-x.1
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (every #'integerp
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 0 nil))))
      (is (every #'integerp
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 1 nil))))
      (is (every #'integerp
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 2 nil)))))))

#+crategus
(test pango-layout-index-to-line-x.2
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '(0 0)
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 0 nil))))
      (is (equal '(0 9216)
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 1 nil))))
      (is (equal '(0 18432)
                 (multiple-value-list
                     (pango:layout-index-to-line-x layout 2 nil)))))))

;;;     pango_layout_xy_to_index

(test pango-layout-xy-to-index
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '(0 0)
                 (multiple-value-list (pango:layout-xy-to-index layout 0 0))))
      (is (equal '(1 0)
                 (multiple-value-list (pango:layout-xy-to-index layout 11264 0))))
      (is (equal '(2 0)
                 (multiple-value-list
                     (pango:layout-xy-to-index layout 21504 0)))))))

;;;     pango_layout_get_cursor_pos

(test pango-layout-cursor-pos.1
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (every #'integerp
                 (flatten (multiple-value-list
                              (pango:layout-cursor-pos layout 0)))))
      (is (every #'integerp
                 (flatten (multiple-value-list
                              (pango:layout-cursor-pos layout 1)))))
      (is (every #'integerp
                 (flatten (multiple-value-list
                              (pango:layout-cursor-pos layout 2))))))))

#+crategus
(test pango-layout-cursor-pos.2
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '((0 0 0 23552) (0 0 0 23552))
                 (multiple-value-list (pango:layout-cursor-pos layout 0))))
      (is (equal '((9216 0 0 23552) (9216 0 0 23552))
                 (multiple-value-list (pango:layout-cursor-pos layout 1))))
      (is (equal '((18432 0 0 23552) (18432 0 0 23552))
                 (multiple-value-list (pango:layout-cursor-pos layout 2)))))))

;;;     pango_layout_move_cursor_visually

(test pango-layout-move-cursor-visually
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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
                     (pango:layout-move-cursor-visually layout t 2 0 1)))))))

;;;     pango_layout_get_extents

#-windows
(test pango-layout-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (ink logical)
      (is-false (pango:layout-extents layout ink logical))
      (is (equal '(0 6144 77824 12288)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 0 77824 23552)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#+windows
(test pango-layout-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (ink logical)
      (is-false (pango:layout-extents layout ink logical))
      (is (equal '(0 3072 84992 13312)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 0 81920 19456)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#-windows
(test pango-layout-extents.1
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (ink)
      (is-false (pango:layout-extents layout ink nil))
      (is (equal '(0 6144 77824 12288)
                 (multiple-value-list (pango:rectangle-to-integer ink)))))))

#+windows
(test pango-layout-extents.1
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (ink)
      (is-false (pango:layout-extents layout ink nil))
      (is (equal '(0 3072 84992 13312)
                 (multiple-value-list (pango:rectangle-to-integer ink)))))))

#-windows
(test pango-layout-extents.2
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (logical)
      (is-false (pango:layout-extents layout nil logical))
      (is (equal '(0 0 77824 23552)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#+windows
(test pango-layout-extents.2
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (logical)
      (is-false (pango:layout-extents layout nil logical))
      (is (equal '(0 0 81920 19456)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

;;;     pango_layout_get_pixel_extents

#-windows
(test pango-layout-pixel-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (ink logical)
      (is-false (pango:layout-pixel-extents layout ink logical))
      (is (equal '(0 6 76 12)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 0 76 23)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#+windows
(test pango-layout-pixel-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (ink logical)
      (is-false (pango:layout-pixel-extents layout ink logical))
      (is (equal '(0 3 83 13)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 0 80 19)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#-windows
(test pango-layout-pixel-extents.1
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (ink)
      (is-false (pango:layout-pixel-extents layout ink nil))
      (is (equal '(0 6 76 12)
                 (multiple-value-list (pango:rectangle-to-integer ink)))))))

#-windows
(test pango-layout-pixel-extents.2
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
    (pango:with-rectangles (logical)
      (is-false (pango:layout-pixel-extents layout nil logical))
      (is (equal '(0 0 76 23)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

;;;     pango_layout_get_size
;;;     pango_layout_get_pixel_size

(test pango-layout-size/pixel-size.1
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (every #'integerp
                 (multiple-value-list (pango:layout-size layout))))
      (is (every #'integerp
                 (multiple-value-list (pango:layout-pixel-size layout)))))))

#+crategus
(test pango-layout-size/pixel-size.2
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (equal '(77824 23552)
                 (multiple-value-list (pango:layout-size layout))))
      (is (equal '(76 23)
                 (multiple-value-list (pango:layout-pixel-size layout)))))))

;;;     pango_layout_get_baseline
;;;     pango_layout_get_line_count

(test pango-layout-baseline/line-count.1
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (integerp (pango:layout-baseline layout)))
      (is (integerp (pango:layout-line-count layout))))))

#+crategus
(test pango-layout-baseline/line-count.2
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (= 18432 (pango:layout-baseline layout)))
      (is (= 30 (pango:layout-line-count layout))))))

;;;     pango_layout_get_line

(test pango-layout-line
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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
            do (is (typep (pango:layout-line layout count)
                          'pango:layout-line))))))

;;;     pango_layout_get_line_readonly

(test pango-layout-line-readonly
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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
                          'pango:layout-line))))))

;;;     pango_layout_get_lines

(test pango-layout-lines
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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
                 (pango:layout-lines layout))))))

;;;     pango_layout_get_lines_readonly

(test pango-layout-lines-readonly
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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
                 (pango:layout-lines-readonly layout))))))

;;;     pango_layout_get_iter

(test pango-layout-iter
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context)))
      (is (typep (pango:layout-iter layout) 'pango:layout-iter))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (typep (pango:layout-iter layout) 'pango:layout-iter)))))

;;;     pango_layout_iter_copy

(test pango-layout-iter-copy
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= "Some text"
                   (setf (pango:layout-text layout) "Some text")))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (typep (pango:layout-iter-copy iter) 'pango:layout-iter)))))

;;;     pango_layout_iter_next_run

(test pango-layout-iter-next-run
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (= 0 (pango:layout-iter-index iter)))
      (is-true (pango:layout-iter-next-run iter))
      (is (= 78 (pango:layout-iter-index iter))))))

;;;     pango_layout_iter_next_char
;;;     pango_layout_iter_get_index

(test pango-layout-iter-next-char
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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
      (is (= 3 (pango:layout-iter-index iter))))))

;;;     pango_layout_iter_next_cluster

(test pango-layout-iter-next-cluster
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
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
      (is (= 2 (pango:layout-iter-index iter))))))

;;;     pango_layout_iter_next_line
;;;     pango_layout_iter_at_last_line
;;;     pango_layout_iter_get_baseline

(test pango-layout-iter-line/next-line/at-last-line.1
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (integerp (pango:layout-iter-index iter)))
      (is (integerp (pango:layout-iter-baseline iter)))
      (is-true (pango:layout-iter-next-line iter))
      (is (integerp (pango:layout-iter-index iter)))
      (is-false (pango:layout-iter-at-last-line iter))
      (is (integerp (pango:layout-iter-baseline iter))))))

#+crategus
(test pango-layout-iter-line/next-line/at-last-line.2
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (= 0 (pango:layout-iter-index iter)))
      (is (= 18432 (pango:layout-iter-baseline iter)))
      (is-true (pango:layout-iter-next-line iter))
      (is (= 79 (pango:layout-iter-index iter)))
      (is-false (pango:layout-iter-at-last-line iter))
      (is (= 41984 (pango:layout-iter-baseline iter))))))

;;;     pango_layout_iter_get_run
;;;     pango_layout_iter_get_run_readonly

(test pango-layout-iter-run
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (typep (pango:layout-iter-run iter) 'pango:glyph-item))
      (is (typep (pango:layout-iter-run-readonly iter) 'pango:glyph-item)))))

;;;     pango_layout_iter_get_line
;;;     pango_layout_iter_get_line_readonly

(test pango-layout-iter-line
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (typep (pango:layout-iter-line iter) 'pango:layout-line))
      (is (typep (pango:layout-iter-line-readonly iter) 'pango:layout-line)))))

;;;     pango_layout_iter_get_layout

(test pango-layout-iter-line
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (eq layout (pango:layout-iter-layout iter))))))

;;;     pango_layout_iter_get_char_extents

#-windows
(test pango-layout-iter-char-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (pango:with-rectangles (ink)
        (is-false (pango:layout-iter-char-extents iter ink))
        (is (equal '(0 0 16384 23552)
                   (multiple-value-list (pango:rectangle-to-integer ink)))))))

#+windows
(test pango-layout-iter-char-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (pango:with-rectangles (ink)
        (is-false (pango:layout-iter-char-extents iter ink))
        (is (equal '(0 0 15360 19456)
                   (multiple-value-list (pango:rectangle-to-integer ink)))))))

;;;     pango_layout_iter_get_cluster_extents

#-windows
(test pango-layout-iter-cluster-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))

      (pango:with-rectangles (ink logical)
        (is-false (pango:layout-iter-cluster-extents iter ink logical))
        (is (equal '(0 6144 17408 12288)
                   (multiple-value-list (pango:rectangle-to-integer ink))))
        (is (equal '(0 0 16384 23552)
                   (multiple-value-list (pango:rectangle-to-integer logical)))))))

#+windows
(test pango-layout-iter-cluster-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))

      (pango:with-rectangles (ink logical)
        (is-false (pango:layout-iter-cluster-extents iter ink logical))
        (is (equal '(-1024 3072 19456 12288)
                   (multiple-value-list (pango:rectangle-to-integer ink))))
        (is (equal '(0 0 15360 19456)
                   (multiple-value-list (pango:rectangle-to-integer logical)))))))

;;;     pango_layout_iter_get_run_extents

(test pango-layout-iter-run-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))

      (pango:with-rectangles (ink logical)
        (is-false (pango:layout-iter-run-extents iter ink logical))
        #-windows
        (is (equal '(0 5120 636928 17408)
                   (multiple-value-list (pango:rectangle-to-integer ink))))
        #+windows
        (is (equal '(-1024 2048 662528 16384)
                   (multiple-value-list (pango:rectangle-to-integer ink))))
        #-windows
        (is (equal '(0 0 636928 23552)
                   (multiple-value-list (pango:rectangle-to-integer logical))))
        #+windows
        (is (equal '(0 0 659456 19456)
                   (multiple-value-list (pango:rectangle-to-integer logical)))))))

;;;     pango_layout_iter_get_line_yrange

(test pango-layout-iter-line-yrange.1
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (every #'integerp
                 (multiple-value-list (pango:layout-iter-line-yrange iter)))))))

#+crategus
(test pango-layout-iter-line-yrange.2
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (iter nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
      (is (equal '(0 23552)
                 (multiple-value-list (pango:layout-iter-line-yrange iter)))))))

;;;     pango_layout_iter_get_line_extents

#-windows
(test pango-layout-iter-line-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
    (is (string= *sample-text-1*
                 (setf (pango:layout-text layout) *sample-text-1*)))
    (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
    (pango:with-rectangles (ink logical)
      (pango:layout-iter-line-extents iter ink logical)
      (is (equal '(0 5120 636928 17408)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 0 636928 23552)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#+windows
(test pango-layout-iter-line-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
    (is (string= *sample-text-1*
                 (setf (pango:layout-text layout) *sample-text-1*)))
    (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
    (pango:with-rectangles (ink logical)
      (pango:layout-iter-line-extents iter ink logical)
      (is (equal '(-1024 2048 662528 16384)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 0 659456 19456)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

;;;     pango_layout_iter_get_layout_extents

#-windows
(test pango-layout-iter-layout-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
    (is (string= *sample-text-1*
                 (setf (pango:layout-text layout) *sample-text-1*)))
    (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
    (pango:with-rectangles (ink logical)
      (pango:layout-iter-layout-extents iter ink logical)
      (is (equal '(-1024 5120 681984 696320)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 0 680960 706560)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#+windows
(test pango-layout-iter-layout-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (iter nil))
    (is (string= *sample-text-1*
                 (setf (pango:layout-text layout) *sample-text-1*)))
    (is (typep (setf iter (pango:layout-iter layout)) 'pango:layout-iter))
    (pango:with-rectangles (ink logical)
      (pango:layout-iter-layout-extents iter ink logical)
      (is (equal '(-1024 2048 709632 578560)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 0 708608 583680)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

;;;     pango_layout_line_get_extents

#-windows
(test pango-layout-line-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (line nil))
    (is (string= *sample-text-1*
                 (setf (pango:layout-text layout) *sample-text-1*)))
    (is (typep (setf line (pango:layout-line layout 0)) 'pango:layout-line))
    (pango:with-rectangles (ink logical)
      (pango:layout-line-extents line ink logical)
      (is (equal '(0 -13312 636928 17408)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 -18432 636928 23552)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#+windows
(test pango-layout-line-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (line nil))
    (is (string= *sample-text-1*
                 (setf (pango:layout-text layout) *sample-text-1*)))
    (is (typep (setf line (pango:layout-line layout 0)) 'pango:layout-line))
    (pango:with-rectangles (ink logical)
      (pango:layout-line-extents line ink logical)
      (is (equal '(-1024 -13312 662528 16384)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 -15360 659456 19456)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

;;;     pango_layout_line_get_pixel_extents

#-windows
(test pango-layout-line-pixel-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (line nil))
    (is (string= *sample-text-1*
                 (setf (pango:layout-text layout) *sample-text-1*)))
    (is (typep (setf line (pango:layout-line layout 0)) 'pango:layout-line))
    (pango:with-rectangles (ink logical)
      (pango:layout-line-pixel-extents line ink logical)
      (is (equal '(0 -13 622 17)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 -18 622 23)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

#+windows
(test pango-layout-line-pixel-extents
  (let* ((fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (layout (pango:layout-new context))
         (line nil))
    (is (string= *sample-text-1*
                 (setf (pango:layout-text layout) *sample-text-1*)))
    (is (typep (setf line (pango:layout-line layout 0)) 'pango:layout-line))
    (pango:with-rectangles (ink logical)
      (pango:layout-line-pixel-extents line ink logical)
      (is (equal '(-1 -13 647 16)
                 (multiple-value-list (pango:rectangle-to-integer ink))))
      (is (equal '(0 -15 644 19)
                 (multiple-value-list (pango:rectangle-to-integer logical)))))))

;;;     pango_layout_line_get_height

(test pango-layout-line-height.1
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (line nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf line (pango:layout-line layout 1)) 'pango:layout-line))
      (is (typep line 'pango:layout-line))
      (is (integerp (pango:layout-line-height line))))))

#+crategus
(test pango-layout-line-height.2
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (line nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))
      (is (typep (setf line (pango:layout-line layout 1)) 'pango:layout-line))
      (is (typep line 'pango:layout-line))
      (is (= 22528 (pango:layout-line-height line))))))

;;;     pango_layout_line_get-length

;;;     pango_layout_line_get-resolved-direction
;;;     pango_layout_line_get-start-index
;;;     pango_layout_line_get_x_ranges

;;;     pango_layout_line_index_to_x
;;;     pango_layout_line_x_to_index

(test pango-layout-line-index-to-x/x-to-index
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (line nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))

      (is (typep (setf line (pango:layout-line layout 1)) 'pango:layout-line))
      (is (integerp (pango:layout-line-index-to-x line 5 nil))))))

#+crategus
(test pango-layout-line-index-to-x/x-to-index
  (cairo:with-context-for-image-surface (cr :rgb24 200 400)
    (let* ((context (pango:cairo-create-context cr))
           (layout (pango:layout-new context))
           (line nil))
      (is (string= *sample-text-1*
                   (setf (pango:layout-text layout) *sample-text-1*)))

      (is (typep (setf line (pango:layout-line layout 1)) 'pango:layout-line))
      (is (= 645120 (pango:layout-line-index-to-x line 5 nil))))))

;;;     pango_layout_line_is-paragraph-start

;;; 2025-1-3
