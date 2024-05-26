(in-package :pango-test)

(def-suite pango-item :in pango-suite)
(in-suite pango-item)

;; Check usage of nested defcstructs
(cffi:defcstruct s1
  (an-int :int))

(cffi:defctype s1 (:struct s1))

(cffi:defcstruct s2
  (an-s1 s1))

(cffi:defctype s2 (:struct s2))

(test struct.nested-setf
  (cffi:with-foreign-object (an-s2 's2)
    (setf (cffi:foreign-slot-value (cffi:foreign-slot-value an-s2 's2 'an-s1)
                                   's1 'an-int)
          1984)
    (is (= 1984
    (cffi:foreign-slot-value (cffi:foreign-slot-value an-s2 's2 'an-s1)
                               's1 'an-int)))))

;;; --- Types and Values -------------------------------------------------------

;;;     PANGO_ANALYSIS_FLAG_CENTERED_BASELINE
;;;     PANGO_ANALYSIS_FLAG_IS_ELLIPSIS
;;;     PANGO_ANALYSIS_FLAG_NEED_HYPHEN

;;;     PangoShapeFlags

(test pango-shape-flags
  ;; Check type
  (is (g:type-is-flags "PangoShapeFlags"))
  ;; Check registered name
  (is (eq 'pango:shape-flags
          (glib:symbol-for-gtype "PangoShapeFlags")))
  ;; Check type initializer
  (is (eq (g:gtype "PangoShapeFlags")
          (g:gtype (cffi:foreign-funcall "pango_shape_flags_get_type" :size))))
  ;; Check names
  (is (equal '("PANGO_SHAPE_NONE" "PANGO_SHAPE_ROUND_POSITIONS")
             (list-flags-item-name "PangoShapeFlags")))
  ;; Check values
  (is (equal '(0 1)
             (list-flags-item-value "PangoShapeFlags")))
  ;; Check nick names
  (is (equal '("none" "round-positions")
             (list-flags-item-nick "PangoShapeFlags")))
  ;; Check flags definition
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

(test pango-item-boxed
  ;; Check type
  (is (g:type-is-boxed "PangoItem"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoItem")
          (g:gtype (cffi:foreign-funcall "pango_item_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:item
          (glib:symbol-for-gtype "PangoItem"))))

(test pango-item-properties
  (let ((item (pango:item-new)))
    (is (cffi:pointerp (pango:item-analysis item)))
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

(test pango-item-split
  (let* ((text "This is some text.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         item item1)
    (is (typep (setf item
                     (first (pango:itemize context
                                           text
                                           0 (length text)
                                           nil nil)))
               'pango:item))
    (is (typep (setf item1
                     (pango:item-split item 5 1))
               'pango:item))))

;;;     pango_item_apply_attrs

(test pango-item-apply-attrs
  (let* ((text "This is some text.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (attrs (pango:attr-list-from-string "5 7 weight bold"))
         (iter (pango:attr-list-iterator attrs))
         items item)
    ;; Itemize the text without attributes
    (is (= 1 (length (setf items
                           (pango:itemize context
                                          text
                                          0 (length text)
                                          (pango:attr-list-from-string "")
                                          nil)))))
    ;; Get the item
    (is (typep (setf item (first items)) 'pango:item))
    ;; No attributes
    (is (equal '()
               (mapcar #'pango:attribute-type
                       (pango:analysis-extra-attrs (pango:item-analysis item)))))
    ;; Apply attribute
    (is-false (pango:item-apply-attrs item iter))
    ;; Check the applied attribute
    (is (equal '(:weight)
               (mapcar #'pango:attribute-type
                       (pango:analysis-extra-attrs (pango:item-analysis item)))))))

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
         item items analysis)
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

    (is (cffi:pointerp (setf analysis (pango:item-analysis item))))
    (is (typep (pango:analysis-font analysis) 'pango:font))
    (is (= 0 (pango:analysis-level analysis)))
    (is (eq :south (pango:analysis-gravity analysis)))
    (is (= 128 (pango:analysis-flags analysis)))
    (is (eq :latin (pango:analysis-script analysis)))
    (is (typep (pango:analysis-language analysis) 'pango:language))
    (is-false (pango:analysis-extra-attrs analysis))

    (setf item (second items))
    (is (= 5 (pango:item-offset item)))
    (is (= 2 (pango:item-length item)))
    (is (= 2 (pango:item-num-chars item)))

    (is (cffi:pointerp (setf analysis (pango:item-analysis item))))
    (is (typep (pango:analysis-font analysis) 'pango:font))
    (is (= 0 (pango:analysis-level analysis)))
    (is (eq :south (pango:analysis-gravity analysis)))
    (is (= 128 (pango:analysis-flags analysis)))
    (is (eq :latin (pango:analysis-script analysis)))
    (is (typep (pango:analysis-language analysis) 'pango:language))
    (is-false (pango:analysis-extra-attrs analysis))

    (setf item (third items))
    (is (= 7 (pango:item-offset item)))
    (is (= 1 (pango:item-length item)))
    (is (= 1 (pango:item-num-chars item)))

    (is (cffi:pointerp (setf analysis (pango:item-analysis item))))
    (is (typep (pango:analysis-font analysis) 'pango:font))
    (is (= 0 (pango:analysis-level analysis)))
    (is (eq :south (pango:analysis-gravity analysis)))
    (is (= 128 (pango:analysis-flags analysis)))
    (is (eq :latin (pango:analysis-script analysis)))
    (is (typep (pango:analysis-language analysis) 'pango:language))
    (is-false (pango:analysis-extra-attrs analysis))

    (setf item (fourth items))
    (is (= 8 (pango:item-offset item)))
    (is (= 4 (pango:item-length item)))
    (is (= 4 (pango:item-num-chars item)))

    (is (cffi:pointerp (setf analysis (pango:item-analysis item))))
    (is (typep (pango:analysis-font analysis) 'pango:font))
    (is (= 0 (pango:analysis-level analysis)))
    (is (eq :south (pango:analysis-gravity analysis)))
    (is (= 128 (pango:analysis-flags analysis)))
    (is (eq :latin (pango:analysis-script analysis)))
    (is (typep (pango:analysis-language analysis) 'pango:language))
    (is (equal '(:foreground)
               (mapcar #'pango:attribute-type
                      (pango:analysis-extra-attrs analysis))))))

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

;;;     pango_reorder_items                                not implemented
;;;     pango_break                                        not implemented
;;;     pango_get_log_attrs                                not implemented
;;;     pango_find_paragraph_boundary                      not implemented
;;;     pango_default_break                                not exported
;;;     pango_tailor_break                                 not exported

;;;     pango_shape

(test pango-shape
  (let* ((text "This is some text.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         (attrstr "5 7 weight bold, 8 12 foreground red")
         (attrs (pango:attr-list-from-string attrstr))
         (iter (pango:attr-list-iterator attrs))
         item items analysis)
    (is (= 5 (length (setf items
                           (pango:itemize context
                                          text
                                          0
                                          (babel:string-size-in-octets text)
                                          attrs
                                          iter)))))
    (setf item (first items))
    (is (= 0 (pango:item-offset item)))
    (is (= 5 (pango:item-length item)))
    (is (= 5 (pango:item-num-chars item)))

    (is (cffi:pointerp (setf analysis (pango:item-analysis item))))
    (is (typep (pango:analysis-font analysis) 'pango:font))
    (is (= 0 (pango:analysis-level analysis)))
    (is (eq :south (pango:analysis-gravity analysis)))
    (is (= 128 (pango:analysis-flags analysis)))
    (is (eq :latin (pango:analysis-script analysis)))
    (is (typep (pango:analysis-language analysis) 'pango:language))
    (is-false (pango:analysis-extra-attrs analysis))

    (is (typep (pango:shape (babel:octets-to-string
                                (subseq (babel:string-to-octets text)
                                        (pango:item-offset item)
                                        (+ (pango:item-offset item)
                                           (pango:item-length item))))
                            (pango:item-length item)
                            (pango:item-analysis item))
                'pango:glyph-string))))

;;;     pango_shape_full
;;;     pango_shape_with_flags

;;; 2024-5-25
