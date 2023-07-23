(in-package :pango-test)

(def-suite pango-item :in pango-suite)
(in-suite pango-item)

;;; --- Types and Values -------------------------------------------------------

;;;
;;;     PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;     PANGO_ANALYSIS_FLAG_IS_ELLIPSIS
;;;     PANGO_ANALYSIS_FLAG_NEED_HYPHEN

;;;     PangoShapeFlags

(test pango-shape-flags
  ;; Check the type
  (is (g:type-is-flags "PangoShapeFlags"))
  ;; Check the registered name
  (is (eq 'pango:shape-flags
          (glib:symbol-for-gtype "PangoShapeFlags")))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoShapeFlags")
          (g:gtype (cffi:foreign-funcall "pango_shape_flags_get_type" :size))))
  ;; Check the names
  (is (equal '("PANGO_SHAPE_NONE" "PANGO_SHAPE_ROUND_POSITIONS")
             (list-flags-item-name "PangoShapeFlags")))
  ;; Check the values
  (is (equal '(0 1)
             (list-flags-item-value "PangoShapeFlags")))
  ;; Check the nick names
  (is (equal '("none" "round-positions")
             (list-flags-item-nick "PangoShapeFlags")))
  ;; Check the flags definition
  (is (equal '(GOBJECT:DEFINE-G-FLAGS "PangoShapeFlags" PANGO-SHAPE-FLAGS
                                      (:EXPORT T
                                       :TYPE-INITIALIZER
                                       "pango_shape_flags_get_type")
                                      (:NONE 0)
                                      (:ROUND-POSITIONS 1))
             (gobject:get-g-type-definition "PangoShapeFlags"))))

;;;     PangoLogAttr
;;;     PangoAnalysis

;;;     PangoItem

(test pango-item-struct
  ;; Type check
  (is (g:type-is-a (g:gtype "PangoItem") g:+g-type-boxed+))
  ;; Check the type initializer
  (is (eq (g:gtype "PangoItem")
          (g:gtype (cffi:foreign-funcall "pango_item_get_type" :size)))))

(test pango-item-properties
  (let ((item (make-instance 'pango:item)))
    (is (cffi:null-pointer-p (pango:item-analysis item)))
    (is (= 0 (pango:item-length item)))
    (is (= 0 (pango:item-num-chars item)))
    (is (= 0 (pango:item-offset item)))))

;;; --- Functions --------------------------------------------------------------

;;;     pango_item_new
;;;     pango_item_copy

(test pango-item-new/copy
  (let ((item (pango:item-new)))
    (is (typep item 'pango:item))
    (is (typep (pango:item-copy item) 'pango:item))))

;;;     pango_item_split

;; FIXME: The PANGO:ITEMIZE function returns NULL-POINTERS for 
;; PANGO:ANALYSIS. This causes the PANGO:ITEM-SPLIT function to crash.

#+nil
(test pango-item-split
  (let* ((text "This is some text.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         item item1)
    (is-false (setf item
                    (first (pango:itemize context 
                                          text 
                                          0 (length text) 
                                          nil nil))))
    ;; This call causes a memory fault:
    ;;  Unexpected Error: #<SB-SYS:MEMORY-FAULT-ERROR {1006A41563}>
    ;;  Unhandled memory fault at #xFFFFFFFFFFFFFFFF..
    (is-false (setf item1
                    (pango:item-split item 5 0)))
))

;;;     pango_item_apply_attrs

;;;     pango_itemize

(test pango-itemize.1
  (let* ((text "This is some text.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (attrstr "5 7 weight bold, 8 12 foreground red")
         (attrs (pango:attr-list-from-string attrstr))
         (iter (pango:attr-list-iterator attrs))
         items)
    (is (typep attrs 'pango:attr-list))
    (is (typep iter 'pango:attr-iterator))

    (is (equal '(:weight :foreground)
               (mapcar #'pango:attribute-type
                       (pango:attr-list-attributes attrs))))

    (is (= 5 (length (setf items
                           (pango:itemize context
                                          text
                                          0 (length text)
                                          attrs
                                          iter)))))
    (is (every (lambda (x) (typep x 'pango:item)) items))))

(test pango-itemize.2
  (let* ((text "This is some text.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (attrstr "5 7 weight bold, 8 12 foreground red")
         (attrs (pango:attr-list-from-string attrstr))
         (iter (pango:attr-list-iterator attrs))
         item items)
    (is (= 5 (length (setf items
                           (pango:itemize context
                                          text
                                          0 (length text)
                                          attrs
                                          iter)))))
    (setf item (first items))
    (is (= 0 (pango:item-offset item)))
    (is (= 5 (pango:item-length item)))
    (is (= 5 (pango:item-num-chars item)))
    ;; TODO: Is NULL the expected return value? Is something wrong?
    ;; The NULL-POINTER causes the function PANGO:ITEM-SPLIT to crash.
    (is (cffi:null-pointer-p (pango:item-analysis item)))

    (setf item (second items))
    (is (= 5 (pango:item-offset item)))
    (is (= 2 (pango:item-length item)))
    (is (= 2 (pango:item-num-chars item)))
    ;; TODO: Is NULL the expected return value? Is something wrong?
    ;; The NULL-POINTER causes the function PANGO:ITEM-SPLIT to crash.
    (is (cffi:null-pointer-p (pango:item-analysis item)))))

;;;     pango_itemize_with_base_dir

(test pango-itemize-with-base-dir
  (let* ((text "This is some text.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (attrstr "5 7 weight bold, 8 12 foreground red")
         (attrs (pango:attr-list-from-string attrstr))
         (iter (pango:attr-list-iterator attrs))
         items)
    (is (typep attrs 'pango:attr-list))
    (is (typep iter 'pango:attr-iterator))
    (is (= 5 (length (setf items
                           (pango:itemize-with-base-dir context
                                                        :ltr
                                                        text
                                                        0 (length text)
                                                        attrs
                                                        iter)))))
    (is (every (lambda (x) (typep x 'pango:item)) items))))

;;;     pango_reorder_items
;;;     pango_break
;;;     pango_get_log_attrs
;;;     pango_find_paragraph_boundary
;;;     pango_default_break
;;;     pango_tailor_break
;;;     pango_shape
;;;     pango_shape_full
;;;     pango_shape_with_flags

;;; --- 2023-7-14 --------------------------------------------------------------
