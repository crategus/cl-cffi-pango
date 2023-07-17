(in-package :pango-test)

(def-suite pango-rendering :in pango-suite)
(in-suite pango-rendering)

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


;;;     pango_itemize

;; TODO: Create an example with the ATTRS, ITER arguments different from nil

(test pango-itemize.1
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default))))
    (is (every (lambda (x) (typep x 'pango:item))
               (pango:itemize context "text" 0 (length "text") nil nil)))))

#+nil
(test pango-itemize.2
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default)))
        (attrs (pango:attr-list-new)))

    (is-false (pango:attr-list-insert attrs
                                      (pango:attr-language-new
                                          (pango:language-default))))
    (is-false (pango:attr-list-insert attrs
                                      (pango:attr-family-new "Sans")))
    ;; TODO: Find a more interesting example
    (is (every (lambda (x) (typep x 'pango:item))
               (pango:itemize context "text" 0 (length "text")
                              attrs
                              (pango:attr-list-iterator attrs))))
               ))

;;;     pango_itemize_with_base_dir

;; TODO: Create an example with the ATTRS, ITER arguments different from nil

(test pango-itemize-with-base-dir
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default))))
    (is (every (lambda (x) (typep x 'pango:item))
               (pango:itemize-with-base-dir context
                                            :ltr
                                            "text" 0 (length "text") nil nil)))))

;;;     pango_item_split

#+nil
(test pango-item-split
  (let* ((label (make-instance 'gtk-label))
         (string "this is the text to split")
         (context (gtk-widget-context label))
         (items (itemize context string 5 0 nil nil)))

    (is-false items)
;    (is-false (item-split item 12 0))
;    (is-false item)

))

(test pango-itemize-split
  (let* ((context (pango:font-map-create-context (pango:cairo-font-map-default)))
         (str "this is the text to split.")
         (items (pango:itemize context str 5 0 nil nil)))

    (is-false items)

))

;;;     pango_item_apply_attrs

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
