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
             (glib-test:list-flags-item-names "PangoShapeFlags")))
  ;; Check values
  (is (equal '(0 1)
             (glib-test:list-flags-item-values "PangoShapeFlags")))
  ;; Check nick names
  (is (equal '("none" "round-positions")
             (glib-test:list-flags-item-nicks "PangoShapeFlags")))
  ;; Check flags definition
  (is (equal '(GOBJECT:DEFINE-GFLAGS "PangoShapeFlags" PANGO:SHAPE-FLAGS
                                     (:EXPORT T
                                      :TYPE-INITIALIZER
                                      "pango_shape_flags_get_type")
                                     (:NONE 0)
                                     (:ROUND-POSITIONS 1))
             (gobject:get-gtype-definition "PangoShapeFlags"))))

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

(test pango-item-split.1
  (let* ((text "This is some text.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         items item)
    ;; Itemize the text
    (setf items (pango:itemize context text 0 (length text) nil nil))
    (is (= 1 (length items)))
    (is (= 18 (pango:item-length (first items))))
    (is (= 18 (pango:item-num-chars (first items))))
    (is (=  0 (pango:item-offset (first items))))
    ;; Split the first item in items
    (is (typep (setf item (pango:item-split (first items) 5 5)) 'pango:item))
    ;; Length and offset for split item
    (is (= 5 (pango:item-length item)))
    (is (= 5 (pango:item-num-chars item)))
    (is (= 0 (pango:item-offset item)))
    ;; Length and offset for original item after split
    (is (= 13 (pango:item-length (first items))))
    (is (= 13 (pango:item-num-chars (first items))))
    (is (=  5 (pango:item-offset (first items))))))

(test pango-item-split.2
  (let* ((text "Zwölf Ägypter gehen über die Straße.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         items item)
    ;; Itemize the text
    (setf items (pango:itemize context text 0 (length text) nil nil))
    (is (= 1 (length items)))
    (is (= 36 (pango:item-length (first items))))
    (is (= 33 (pango:item-num-chars (first items))))
    (is (=  0 (pango:item-offset (first items))))
    ;; Split the first item in items
    (is (typep (setf item (pango:item-split (first items) 5 5)) 'pango:item))
    ;; Length and offset for split item
    (is (= 5 (pango:item-length item)))
    (is (= 5 (pango:item-num-chars item)))
    (is (= 0 (pango:item-offset item)))
    ;; Length and offset for original item after split
    (is (= 31 (pango:item-length (first items))))
    (is (= 28 (pango:item-num-chars (first items))))
    (is (=  5 (pango:item-offset (first items))))))

;;;     pango_item_apply_attrs
;;;     pango_item_get_char_offset

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
                                          0 (babel:string-size-in-octets text)
                                          (pango:attr-list-from-string "")
                                          nil)))))
    ;; Get the item
    (is (typep (setf item (first items)) 'pango:item))
    ;; Char offset is 0
    (is (= 0 (pango:item-char-offset item)))
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
                                          0 (babel:string-size-in-octets text)
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
                                          0 (babel:string-size-in-octets text)
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

;; Simpler version with no attributes
(test pango-itemize.3
  (let* ((text "Zwölf Ägypter gehen über die Straße.")
         (fontmap (pango:cairo-font-map-default))
         (context (pango:font-map-create-context fontmap))
         items)
    (is (= 1 (length (setf items
                           (pango:itemize context
                                          text
                                          0 (babel:string-size-in-octets text)
                                          nil
                                          nil)))))
    (is (every (lambda (x) (typep x 'pango:item)) items))))

(test pango-itemize.4
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
                           (pango:itemize context
                                          text
                                          0 (length text)
                                          attrs
                                          iter
                                          :direction :ltr)))))
    (is (every (lambda (x) (typep x 'pango:item)) items))))

;;;     pango_itemize_with_base_dir                         not needed

;;;     pango_reorder_items                                not implemented
;;;     pango_break                                        not implemented
;;;     pango_get_log_attrs                                not implemented

;;;     pango_find_paragraph_boundary                      not implemented

(test pango-find-paragraph-boundary.1
  (let* ((text *sample-text-1*))

    (is (equal '(78 79)
               (multiple-value-list (pango:find-paragraph-boundary text))))

    ;; First line of text
    (multiple-value-bind (index next)
        (pango:find-paragraph-boundary text)
      (is (string= "Weit hinten, hinter den Wortbergen, fern der Länder Vokalien und Konsonantien"
                   (subseq text 0 (1- index))))
      (is (= 1788 (length (setf text (subseq text (1- next)))))))
    ;; Second line of text
    (multiple-value-bind (index next)
        (pango:find-paragraph-boundary text)
      (is (string= "leben die Blindtexte. Abgeschieden wohnen Sie in Buchstabenhausen an der Küste"
                   (subseq text 0 (1- index))))
      (is (= 1709 (length (setf text (subseq text (1- next)))))))
))

(test pango-find-paragraph-boundary.2
  (let ((text *sample-text-1*))
    (iter (while (not (string= "" text)))
          (multiple-value-bind (index next)
              (pango:find-paragraph-boundary text)
            (format t "~a,~a : ~a~%~%" index next (subseq text 0 index))
            (setf text
                  (babel:octets-to-string (subseq (babel:string-to-octets text)
                                                   next)))))
))

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
                            (pango:item-analysis item))
                'pango:glyph-string))))

;; Finish this example, handle line breaks
(test pango-shape.2
  (cairo:with-context-for-image-surface (cr :argb32 800 1200)
    (let* ((text *sample-text-1*)
           (path (glib-sys:sys-path "test/out/pango-shape-2.png"))
           (context (pango:cairo-create-context cr))
           items text1
;          (attrstr "5 7 weight bold, 8 12 foreground red")
;          (attrs (pango:attr-list-from-string attrstr))
;          (iter (pango:attr-list-iterator attrs))
;          (items (pango:itemize context
;                                text
;                                0
;                                (babel:string-size-in-octets text)
;                                attrs
;                                iter))
                                 )
      ;; Clear the background
      (cairo:set-source-rgb cr 1.0 1.0 1.0)
      (cairo:paint cr)
      ;; Set the color
      (cairo:set-source-rgb cr 0.5 0.5 0.5)
      ;; Move to the start position of the text
      (cairo:move-to cr 24 36)

      (iter (while (not (string= "" text)))
        (multiple-value-bind (index next)
            (pango:find-paragraph-boundary text)

          (format t "~&~a,~a : ~a~%~%" index next (subseq text 0 index))

          (setf text1 (subseq text 0 index))
          (setf items (pango:itemize context
                                     text1
                                     0
                                     (babel:string-size-in-octets text1)
                                     nil
                                     nil))
          (dolist (item items)
            (let ((font (pango:analysis-font (pango:item-analysis item)))
                  (glyphs (pango:shape (babel:octets-to-string
                                              (subseq (babel:string-to-octets text1)
                                                      (pango:item-offset item)
                                                      (+ (pango:item-offset item)
                                                         (pango:item-length item))))
                                            (pango:item-analysis item))))
              ;; Print the text on the Cario context
              (pango:cairo-show-glyph-string cr font glyphs)
              (cairo:rel-move-to cr 0 24)))

            (setf text
                  (babel:octets-to-string (subseq (babel:string-to-octets text)
                                                   next)))))

      ;; Create and save the PNG image
      (cairo:surface-write-to-png (cairo:target cr) path))))

;;;     pango_shape_full
;;;     pango_shape_with_flags

;;; 2026-03-23
