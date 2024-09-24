(in-package :pango-test)

(def-suite pango-renderer :in pango-suite)
(in-suite pango-renderer)

;;; --- Types and Values -------------------------------------------------------

;;;     PangoRenderPart

(test pango-render-part
  ;; Check type
  (is (g:type-is-enum "PangoRenderPart"))
  ;; Check type initializer
  (is (eq (g:gtype "PangoRenderPart")
          (g:gtype (cffi:foreign-funcall "pango_render_part_get_type" :size))))
  ;; Check registered name
  (is (eq 'pango:render-part
          (glib:symbol-for-gtype "PangoRenderPart")))
  ;; Check names
  (is (equal '("PANGO_RENDER_PART_FOREGROUND" "PANGO_RENDER_PART_BACKGROUND"
               "PANGO_RENDER_PART_UNDERLINE" "PANGO_RENDER_PART_STRIKETHROUGH"
               "PANGO_RENDER_PART_OVERLINE")
             (glib-test:list-enum-item-names "PangoRenderPart")))
  ;; Check values
  (is (equal '(0 1 2 3 4)
             (glib-test:list-enum-item-values "PangoRenderPart")))
  ;; Check nick names
  (is (equal '("foreground" "background" "underline" "strikethrough" "overline")
             (glib-test:list-enum-item-nicks "PangoRenderPart")))
  ;; Check enum definition
  (is (equal '(GOBJECT:DEFINE-GENUM "PangoRenderPart" PANGO:RENDER-PART
                                    (:EXPORT T
                                     :TYPE-INITIALIZER
                                     "pango_render_part_get_type")
                                    (:FOREGROUND 0)
                                    (:BACKGROUND 1)
                                    (:UNDERLINE 2)
                                    (:STRIKETHROUGH 3)
                                    (:OVERLINE 4))
             (gobject:get-gtype-definition "PangoRenderPart"))))

;;;     PangoRenderer

(test pango-renderer-class
  ;; Check type
  (is (g:type-is-object "PangoRenderer"))
  ;; Check registered name
  (is (eq 'pango:renderer
          (glib:symbol-for-gtype "PangoRenderer")))
  ;; Check type initializer
  (is (eq (g:gtype "PangoRenderer")
          (g:gtype (cffi:foreign-funcall "pango_renderer_get_type" :size))))
  ;; Check parent
  (is (eq (g:gtype "GObject")
          (g:type-parent "PangoRenderer")))
  ;; Check children
  (if *first-run-pango-test*
      (is (equal '()
                 (glib-test:list-children "PangoRenderer")))
      (is (equal '("PangoCairoRenderer")
                 (glib-test:list-children "PangoRenderer"))))
  ;; Check interfaces
  (is (equal '()
             (glib-test:list-interfaces "PangoRenderer")))
  ;; Check properties
  (is (equal '()
             (glib-test:list-properties "PangoRenderer")))
  ;; Check signals
  (is (equal '()
             (glib-test:list-signals "PangoRenderer")))
  ;; Check class definition
  (is (equal '(GOBJECT:DEFINE-GOBJECT "PangoRenderer" PANGO:RENDERER
                       (:SUPERCLASS G:OBJECT
                        :EXPORT T
                        :INTERFACES NIL
                        :TYPE-INITIALIZER "pango_renderer_get_type")
                       NIL)
             (gobject:get-gtype-definition "PangoRenderer"))))

;;;     PangoRendererClass

;;; --- Functions --------------------------------------------------------------

;;;     pango_renderer_draw_layout
;;;     pango_renderer_draw_layout_line
;;;     pango_renderer_draw_glyphs
;;;     pango_renderer_draw_glyph_item
;;;     pango_renderer_draw_rectangle
;;;     pango_renderer_draw_error_underline
;;;     pango_renderer_draw_trapezoid
;;;     pango_renderer_draw_glyph
;;;     pango_renderer_activate
;;;     pango_renderer_deactivate
;;;     pango_renderer_part_changed
;;;     pango_renderer_set_color
;;;     pango_renderer_get_color
;;;     pango_renderer_set_alpha
;;;     pango_renderer_get_alpha
;;;     pango_renderer_set_matrix
;;;     pango_renderer_get_matrix
;;;     pango_renderer_get_layout
;;;     pango_renderer_get_layout_line

;;; 2024-9-18
