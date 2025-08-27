;;; ----------------------------------------------------------------------------
;;; pango.renderer.lisp
;;;
;;; The documentation in this file is taken from the Pango Reference Manual
;;; version 1.56 and modified to document the Lisp binding to the Pango
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; PangoRenderer
;;;
;;;     Rendering driver base class
;;;
;;; Types and Values
;;;
;;;     PangoRenderer
;;;     PangoRenderPart
;;;
;;;     PangoRendererClass
;;;
;;; Functions
;;;
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
;;;
;;; Object Hierarchy
;;;
;;;   GObject
;;;   ╰── PangoRenderer
;;;       ╰── PangoXftRenderer
;;;
;;;   GEnum
;;;   ╰── PangoRenderPart
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoRenderPart
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoRenderPart" render-part
  (:export t
   :type-initializer "pango_render_part_get_type")
  (:foreground 0)
  (:background 1)
  (:underline 2)
  (:strikethrough 3)
  (:overline 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'render-part)
      "GEnum"
      (liber:symbol-documentation 'render-part)
 "@version{2025-08-24}
  @begin{declaration}
(gobject:define-genum \"PangoRenderPart\" pango:render-part
  (:export t
   :type-initializer \"pango_render_part_get_type\")
  (:foreground 0)
  (:background 1)
  (:underline 2)
  (:strikethrough 3)
  (:overline 4))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:foreground]{The text itself.}
      @entry[:background]{The area behind the text.}
      @entry[:underline]{Underlines.}
      @entry[:strikethrough]{Strikethrough lines.}
      @entry[:overline]{Overlines.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{pango:render-part} enumeration defines different items to render
    for such purposes as setting colors.
  @end{short}
  @see-class{pango:renderer}")

;;; ----------------------------------------------------------------------------
;;; struct PangoRenderer
;;;
;;; struct PangoRenderer {
;;;   PangoMatrix *matrix;        /* May be NULL */
;;; };
;;;
;;; PangoRenderer is a base class for objects that are used to render Pango
;;; objects such as PangoGlyphString and PangoLayout.
;;;
;;; PangoMatrix *matrix;
;;;     the current transformation matrix for the Renderer; may be NULL, which
;;;     should be treated the same as the identity matrix.
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "PangoRenderer" renderer
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "pango_renderer_get_type")
  nil)

#+liber-documentation
(setf (documentation 'renderer 'type)
 "@version{2024-02-24}
  @begin{short}
    The @class{pango:renderer} class is a base class that contains the necessary
    logic for rendering @class{pango:layout} or a @class{pango:layout-line}
     objects.
  @end{short}
  By subclassing the @class{pango:renderer} class and overriding operations such
  as @code{draw_glyphs} and @code{draw_rectangle}, renderers for particular
  font backends and destinations can be created.
  @see-class{pango:layout}
  @see-class{pango:layout-line}")

;;; ----------------------------------------------------------------------------
;;; struct PangoRendererClass
;;;
;;; struct PangoRendererClass {
;;;   /* All of the following have default implementations
;;;    * and take as coordinates user coordinates in Pango units
;;;    */
;;;   void (*draw_glyphs) (PangoRenderer     *renderer,
;;;                PangoFont         *font,
;;;                PangoGlyphString  *glyphs,
;;;                int                x,
;;;                int                y);
;;;   void (*draw_rectangle) (PangoRenderer     *renderer,
;;;               PangoRenderPart    part,
;;;               int                x,
;;;               int                y,
;;;               int                width,
;;;               int                height);
;;;   void (*draw_error_underline) (PangoRenderer     *renderer,
;;;                 int                x,
;;;                 int                y,
;;;                 int                width,
;;;                 int                height);
;;;
;;;   /* Nothing is drawn for shaped glyphs unless this is implemented */
;;;   void (*draw_shape) (PangoRenderer  *renderer,
;;;               PangoAttrShape *attr,
;;;               int             x,
;;;               int             y);
;;;
;;;   /* These two must be implemented and take coordinates in
;;;    * device space as doubles.
;;;    */
;;;   void (*draw_trapezoid) (PangoRenderer  *renderer,
;;;               PangoRenderPart part,
;;;               double          y1_,
;;;               double          x11,
;;;               double          x21,
;;;               double          y2,
;;;               double          x12,
;;;               double          x22);
;;;   void (*draw_glyph) (PangoRenderer *renderer,
;;;               PangoFont     *font,
;;;               PangoGlyph     glyph,
;;;               double         x,
;;;               double         y);
;;;
;;;   /* Notification of change in rendering attributes
;;;    */
;;;   void (*part_changed) (PangoRenderer   *renderer,
;;;             PangoRenderPart  part);
;;;
;;;   /* Paired around drawing operations
;;;    */
;;;   void (*begin) (PangoRenderer *renderer);
;;;   void (*end)   (PangoRenderer *renderer);
;;;
;;;   /* Hooks into the details of layout rendering
;;;    */
;;;   void (*prepare_run) (PangoRenderer  *renderer,
;;;                PangoLayoutRun *run);
;;;
;;;   /* All of the following have default implementations
;;;    * and take as coordinates user coordinates in Pango units
;;;    */
;;;   void (*draw_glyph_item) (PangoRenderer     *renderer,
;;;                const char        *text,
;;;                PangoGlyphItem    *glyph_item,
;;;                int                x,
;;;                int                y);
;;; };
;;;
;;; Class structure for PangoRenderer.
;;;
;;; draw_glyphs ()
;;;     draws a PangoGlyphString
;;;
;;; draw_rectangle ()
;;;     draws a rectangle
;;;
;;; draw_error_underline ()
;;;     draws a squiggly line that approximately covers the given rectangle in
;;;     the style of an underline used to indicate a spelling error.
;;;
;;; draw_shape ()
;;;     draw content for a glyph shaped with PangoAttrShape. x, y are the
;;;     coordinates of the left edge of the baseline, in user coordinates.
;;;
;;; draw_trapezoid ()
;;;     draws a trapezoidal filled area
;;;
;;; draw_glyph ()
;;;     draws a single glyph
;;;
;;; part_changed ()
;;;     do renderer specific processing when rendering attributes change
;;;
;;; begin ()
;;;     Do renderer-specific initialization before drawing
;;;
;;; end ()
;;;     Do renderer-specific cleanup after drawing
;;;
;;; prepare_run ()
;;;     updates the renderer for a new run
;;;
;;; draw_glyph_item ()
;;;     draws a PangoGlyphItem
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_draw_layout ()
;;;
;;; void pango_renderer_draw_layout (PangoRenderer *renderer,
;;;                                  PangoLayout *layout,
;;;                                  int x,
;;;                                  int y);
;;;
;;; Draws layout with the specified PangoRenderer.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; layout :
;;;     a PangoLayout
;;;
;;; x :
;;;     X position of left edge of baseline, in user space coordinates in
;;;     Pango units.
;;;
;;; y :
;;;     Y position of left edge of baseline, in user space coordinates in
;;;     Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_draw_layout_line ()
;;;
;;; void pango_renderer_draw_layout_line (PangoRenderer *renderer,
;;;                                       PangoLayoutLine *line,
;;;                                       int x,
;;;                                       int y);
;;;
;;; Draws line with the specified PangoRenderer.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; line :
;;;     a PangoLayoutLine
;;;
;;; x :
;;;     X position of left edge of baseline, in user space coordinates in
;;;     Pango units.
;;;
;;; y :
;;;     Y position of left edge of baseline, in user space coordinates in
;;;     Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_draw_glyphs ()
;;;
;;; void pango_renderer_draw_glyphs (PangoRenderer *renderer,
;;;                                  PangoFont *font,
;;;                                  PangoGlyphString *glyphs,
;;;                                  int x,
;;;                                  int y);
;;;
;;; Draws the glyphs in glyphs with the specified PangoRenderer.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; font :
;;;     a PangoFont
;;;
;;; glyphs :
;;;     a PangoGlyphString
;;;
;;; x :
;;;     X position of left edge of baseline, in user space coordinates in
;;;     Pango units.
;;;
;;; y :
;;;     Y position of left edge of baseline, in user space coordinates in
;;;     Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_draw_glyph_item ()
;;;
;;; void pango_renderer_draw_glyph_item (PangoRenderer *renderer,
;;;                                      const char *text,
;;;                                      PangoGlyphItem *glyph_item,
;;;                                      int x,
;;;                                      int y);
;;;
;;; Draws the glyphs in glyph_item with the specified PangoRenderer, embedding
;;; the text associated with the glyphs in the output if the output format
;;; supports it (PDF for example).
;;;
;;; Note that text is the start of the text for layout, which is then indexed
;;; by glyph_item->item->offset.
;;;
;;; If text is NULL, this simply calls pango_renderer_draw_glyphs().
;;;
;;; The default implementation of this method simply falls back to
;;; pango_renderer_draw_glyphs().
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; text :
;;;     the UTF-8 text that glyph_item refers to, or NULL. [allow-none]
;;;
;;; glyph_item :
;;;     a PangoGlyphItem
;;;
;;; x :
;;;     X position of left edge of baseline, in user space coordinates in
;;;     Pango units.
;;;
;;; y :
;;;     Y position of left edge of baseline, in user space coordinates in
;;;     Pango units.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_draw_rectangle ()
;;;
;;; void pango_renderer_draw_rectangle (PangoRenderer *renderer,
;;;                                     PangoRenderPart part,
;;;                                     int x,
;;;                                     int y,
;;;                                     int width,
;;;                                     int height);
;;;
;;; Draws an axis-aligned rectangle in user space coordinates with the specified
;;; PangoRenderer.
;;;
;;; This should be called while renderer is already active. Use
;;; pango_renderer_activate() to activate a renderer.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; part :
;;;     type of object this rectangle is part of
;;;
;;; x :
;;;     X position at which to draw rectangle, in user space coordinates in
;;;     Pango units
;;;
;;; y :
;;;     Y position at which to draw rectangle, in user space coordinates in
;;;     Pango units
;;;
;;; width :
;;;     width of rectangle in Pango units in user space coordinates
;;;
;;; height :
;;;     height of rectangle in Pango units in user space coordinates
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_draw_error_underline ()
;;;
;;; void
;;; pango_renderer_draw_error_underline (PangoRenderer *renderer,
;;;                                      int x,
;;;                                      int y,
;;;                                      int width,
;;;                                      int height);
;;;
;;; Draw a squiggly line that approximately covers the given rectangle in the
;;; style of an underline used to indicate a spelling error. (The width of the
;;; underline is rounded to an integer number of up/down segments and the
;;; resulting rectangle is centered in the original rectangle)
;;;
;;; This should be called while renderer is already active. Use
;;; pango_renderer_activate() to activate a renderer.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; x :
;;;     X coordinate of underline, in Pango units in user coordinate system
;;;
;;; y :
;;;     Y coordinate of underline, in Pango units in user coordinate system
;;;
;;; width :
;;;     width of underline, in Pango units in user coordinate system
;;;
;;; height :
;;;     height of underline, in Pango units in user coordinate system
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_draw_trapezoid ()
;;;
;;; void
;;; pango_renderer_draw_trapezoid (PangoRenderer *renderer,
;;;                                PangoRenderPart part,
;;;                                double y1_,
;;;                                double x11,
;;;                                double x21,
;;;                                double y2,
;;;                                double x12,
;;;                                double x22);
;;;
;;; Draws a trapezoid with the parallel sides aligned with the X axis using the
;;; given PangoRenderer; coordinates are in device space.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; part :
;;;     type of object this trapezoid is part of
;;;
;;; y1_ :
;;;     Y coordinate of top of trapezoid
;;;
;;; x11 :
;;;     X coordinate of left end of top of trapezoid
;;;
;;; x21 :
;;;     X coordinate of right end of top of trapezoid
;;;
;;; y2 :
;;;     Y coordinate of bottom of trapezoid
;;;
;;; x12 :
;;;     X coordinate of left end of bottom of trapezoid
;;;
;;; x22 :
;;;     X coordinate of right end of bottom of trapezoid
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_draw_glyph ()
;;;
;;; void
;;; pango_renderer_draw_glyph (PangoRenderer *renderer,
;;;                            PangoFont *font,
;;;                            PangoGlyph glyph,
;;;                            double x,
;;;                            double y);
;;;
;;; Draws a single glyph with coordinates in device space.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; font :
;;;     a PangoFont
;;;
;;; glyph :
;;;     the glyph index of a single glyph
;;;
;;; x :
;;;     X coordinate of left edge of baseline of glyph
;;;
;;; y :
;;;     Y coordinate of left edge of baseline of glyph
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_activate ()
;;;
;;; void
;;; pango_renderer_activate (PangoRenderer *renderer);
;;;
;;; Does initial setup before rendering operations on renderer.
;;; pango_renderer_deactivate() should be called when done drawing. Calls such
;;; as pango_renderer_draw_layout() automatically activate the layout before
;;; drawing on it. Calls to pango_renderer_activate() and
;;; pango_renderer_deactivate() can be nested and the renderer will only be
;;; initialized and deinitialized once.
;;;
;;; renderer :
;;;     a PangoRenderer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_deactivate ()
;;;
;;; void
;;; pango_renderer_deactivate (PangoRenderer *renderer);
;;;
;;; Cleans up after rendering operations on renderer. See docs for
;;; pango_renderer_activate().
;;;
;;; renderer :
;;;     a PangoRenderer
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_part_changed ()
;;;
;;; void
;;; pango_renderer_part_changed (PangoRenderer *renderer,
;;;                              PangoRenderPart part);
;;;
;;; Informs Pango that the way that the rendering is done for part has changed
;;; in a way that would prevent multiple pieces being joined together into one
;;; drawing call. For instance, if a subclass of PangoRenderer was to add a
;;; stipple option for drawing underlines, it needs to call
;;;
;;; pango_renderer_part_changed (render, PANGO_RENDER_PART_UNDERLINE);
;;;
;;; When the stipple changes or underlines with different stipples might be
;;; joined together. Pango automatically calls this for changes to colors. (See
;;; pango_renderer_set_color())
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; part :
;;;     the part for which rendering has changed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_set_color ()
;;;
;;; void
;;; pango_renderer_set_color (PangoRenderer *renderer,
;;;                           PangoRenderPart part,
;;;                           const PangoColor *color);
;;;
;;; Sets the color for part of the rendering.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; part :
;;;     the part to change the color of
;;;
;;; color :
;;;     the new color or NULL to unset the current color. [allow-none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_get_color ()
;;;
;;; PangoColor *
;;; pango_renderer_get_color (PangoRenderer *renderer,
;;;                           PangoRenderPart part);
;;;
;;; Gets the current rendering color for the specified part.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; part :
;;;     the part to get the color for
;;;
;;; Returns :
;;;     the color for the specified part, or NULL if it hasn't been set and
;;;     should be inherited from the environment.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_set_alpha ()
;;;
;;; void
;;; pango_renderer_set_alpha (PangoRenderer *renderer,
;;;                           PangoRenderPart part,
;;;                           guint16 alpha);
;;;
;;; Sets the alpha for part of the rendering. Note that the alpha may only be
;;; used if a color is specified for part as well.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; part :
;;;     the part to set the alpha for
;;;
;;; alpha :
;;;     an alpha value between 1 and 65536, or 0 to unset the alpha
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_get_alpha ()
;;;
;;; guint16
;;; pango_renderer_get_alpha (PangoRenderer *renderer,
;;;                           PangoRenderPart part);
;;;
;;; Gets the current alpha for the specified part.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; part :
;;;     the part to get the alpha for
;;;
;;; Returns :
;;;     the alpha for the specified part, or 0 if it hasn't been set and should
;;;     be inherited from the environment.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_set_matrix ()
;;;
;;; void
;;; pango_renderer_set_matrix (PangoRenderer *renderer,
;;;                            const PangoMatrix *matrix);
;;;
;;; Sets the transformation matrix that will be applied when rendering.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; matrix :
;;;     a PangoMatrix, or NULL to unset any existing matrix. (No matrix set is
;;;     the same as setting the identity matrix.). [allow-none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_get_matrix ()
;;;
;;; const PangoMatrix *
;;; pango_renderer_get_matrix (PangoRenderer *renderer);
;;;
;;; Gets the transformation matrix that will be applied when rendering. See
;;; pango_renderer_set_matrix().
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; Returns :
;;;     the matrix, or NULL if no matrix has been set (which is the same as the
;;;     identity matrix). The returned matrix is owned by Pango and must not be
;;;     modified or freed.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_get_layout ()
;;;
;;; PangoLayout *
;;; pango_renderer_get_layout (PangoRenderer *renderer);
;;;
;;; Gets the layout currently being rendered using renderer. Calling this
;;; function only makes sense from inside a subclass's methods, like in its
;;; draw_shape() for example.
;;;
;;; The returned layout should not be modified while still being rendered.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; Returns :
;;;     the layout, or NULL if no layout is being rendered using renderer at
;;;     this time. [transfer none]
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_renderer_get_layout_line ()
;;;
;;; PangoLayoutLine *
;;; pango_renderer_get_layout_line (PangoRenderer *renderer);
;;;
;;; Gets the layout line currently being rendered using renderer. Calling this
;;; function only makes sense from inside a subclass's methods, like in its
;;; draw_shape() for example.
;;;
;;; The returned layout line should not be modified while still being rendered.
;;;
;;; renderer :
;;;     a PangoRenderer
;;;
;;; Returns :
;;;     The layout line, or NULL if no layout line is being rendered using
;;;     renderer at this time.
;;; ----------------------------------------------------------------------------

;;; --- End of file pango.renderer.lisp ----------------------------------------
