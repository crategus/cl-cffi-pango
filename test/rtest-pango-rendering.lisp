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

#+nil
(test itemize
  (let* ((label (make-instance 'gtk-label :label "Text"))
;         (string "text text text")
         (context (gtk-widget-pango-context label)))

    (is (typep context 'context))

    (is (typep (context-font-map context) 'font-map))
    (is (typep (context-font-description context) 'font-description))
    (is (typep (context-language context) 'language))
    (is (eq :ltr (context-base-dir context)))
    (is (eq :south (context-base-gravity context)))
    (is (eq :south (context-gravity context)))

    (is (eq :natural (context-gravity-hint context)))



;    (is-false (itemize context string 0 (length string) nil nil))
;    (is (every (lambda (x) (typep x 'item))
;               (itemize context string 0 (length string) nil nil)))
))

;;;     pango_itemize_with_base_dir

#+nil
(test pango-itemize-with-base-dir
  (let* ((label (make-instance 'gtk-label))
         (string "text text text")
         (context (gtk-widget-pango-context label))
         (attrs (pango::%attr-list-new)))
    (is (typep context 'context))
    (is (every (lambda (x) (typep x 'item))
               (itemize-with-base-dir context
                                            :ltr
                                            string
                                            0 (length string) attrs nil)))
))

;;;     pango_item_free

;;;     pango_item_copy

(test item-copy
  (is (typep (pango:item-copy (pango:item-new)) 'pango:item)))

;;;     pango_item_new

(test item-new
  (is (typep (pango:item-new) 'pango:item)))

;;;     pango_item_split

#+nil
(test pango-item-split
  (let* ((label (make-instance 'gtk-label))
         (string "this is the text to split")
         (context (gtk-widget-context label))
         (items (itemize context string 0 (length string) nil nil)))

    (is-false items)
;    (is-false (item-split item 12 0))
;    (is-false item)

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
