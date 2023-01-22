(in-package :pango-test)

(def-suite pango-rendering :in pango-suite)
(in-suite pango-rendering)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoItem
;;;     PangoAnalysis
;;;
;;;     PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;     PANGO_ANALYSIS_FLAG_IS_ELLIPSIS
;;;     PANGO_ANALYSIS_FLAG_NEED_HYPHEN
;;;
;;;     PangoLogAttr
;;;     PangoShapeFlags

;;; --- Functions --------------------------------------------------------------

;;;     pango_itemize

;; TODO: Create an example with the ATTRS, ITER arguments different from nil

(test itemize.1
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default))))
    (is (every (lambda (x) (typep x 'pango:item))
               (pango:itemize context "text" 0 (length "text") nil nil)))))

#+nil
(test itemize.2
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

(test itemize-with-base-dir
  (let ((context (pango:font-map-create-context (pango:cairo-font-map-default))))
    (is (every (lambda (x) (typep x 'pango:item))
               (pango:itemize-with-base-dir context
                                            :ltr
                                            "text" 0 (length "text") nil nil)))))

;;;     pango_item_new
;;;     pango_item_copy

(test item-new/copy/free
  (let ((item (pango:item-new)))
    (is (typep item 'pango:item))
    (is (typep (pango:item-copy item) 'pango:item))))

;;;     pango_item_free

;; not exported

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


(test itemize-split
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

;;; 2021-1-11
