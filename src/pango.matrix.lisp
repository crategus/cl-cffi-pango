;;; ----------------------------------------------------------------------------
;;; pango.matrix.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.51 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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

;;; Types and Values
;;;
;;;     PANGO_SCALE
;;;
;;;     PangoRectangle
;;;     PangoMatrix
;;;
;;; Functions
;;;
;;;     PANGO_MATRIX_INIT
;;;
;;;     pango_matrix_copy
;;;     pango_matrix_free
;;;     pango_matrix_translate
;;;     pango_matrix_scale
;;;     pango_matrix_rotate
;;;     pango_matrix_concat
;;;     pango_matrix_transform_point
;;;     pango_matrix_transform_distance
;;;     pango_matrix_transform_rectangle
;;;     pango_matrix_transform_pixel_rectangle
;;;     pango_matrix_get_font_scale_factor
;;;     pango_matrix_get_font_scale_factors
;;;     pango_matrix_get_slant_ratio                       Since 1.50
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── PangoMatrix
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; struct PangoRectangle
;;; ----------------------------------------------------------------------------

;; TODO: Consider to implement pango:rectangle more lispy!?

(cffi:defcstruct rectangle
  (x :int)
  (y :int)
  (width :int)
  (height :int))

#+liber-documentation
(setf (liber:alias-for-symbol 'rectangle)
      "CStruct"
      (liber:symbol-documentation 'rectangle)
 "@version{2024-2-29}
  @begin{short}
    The @symbol{pango:rectangle} structure represents a rectangle.
  @end{short}
  It is frequently used to represent the logical or ink extents of a single
  glyph or section of text. See, for instance, the
  @fun{pango:font-glyph-extents} function.
  @begin{pre}
(cffi:defcstruct rectangle
  (x :int)
  (y :int)
  (width :int)
  (height :int))
  @end{pre}
  @begin[code]{table}
    @entry[x]{The x coordinate of the left side of the rectangle.}
    @entry[y]{The y coordinate of the the top side of the rectangle.}
    @entry[width]{The width of the rectangle.}
    @entry[height]{The height of the rectangle.}
  @end{table}
  @see-function{pango:font-glyph-extents}")

(export 'rectangle)

;;; --- pango:rectangle-x ------------------------------------------------------

(defun (setf rectangle-x) (value rect)
  (setf (cffi:foreign-slot-value rect '(:struct rectangle) 'x) value))

(defun rectangle-x (rect)
  (cffi:foreign-slot-value rect '(:struct rectangle) 'x))

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-x)
      "Accessor"
      (documentation 'rectangle-x 'function)
 "@version{2024-3-1}
  @syntax{(pango:rectangle-x instance) => x}
  @syntax{(setf (pango:rectangle-x instance) x)}
  @argument[instance]{a @class{pango:rectangle} instance}
  @argument[x]{an integer with the x component of the rectangle}
  @begin{short}
    Accessor of the @code{x} slot of the @class{pango:rectangle} structure.
  @end{short}
  @see-class{pango:rectangle}")

(export 'rectangle-x)

;;; --- pango:rectangle-y ------------------------------------------------------

(defun (setf rectangle-y) (value rect)
  (setf (cffi:foreign-slot-value rect '(:struct rectangle) 'y) value))

(defun rectangle-y (rect)
  (cffi:foreign-slot-value rect '(:struct rectangle) 'y))

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-y)
      "Accessor"
      (documentation 'rectangle-y 'function)
 "@version{2024-3-1}
  @syntax{(pango:rectangle-y instance) => y}
  @syntax{(setf (pango:rectangle-y instance) y)}
  @argument[instance]{a @class{pango:rectangle} instance}
  @argument[y]{an integer with the y component of the rectangle}
  @begin{short}
    Accessor of the @code{y} slot of the @class{pango:rectangle} structure.
  @end{short}
  @see-class{pango:rectangle}")

(export 'rectangle-y)

;;; --- pango:rectangle-width --------------------------------------------------

(defun (setf rectangle-width) (value rect)
  (setf (cffi:foreign-slot-value rect '(:struct rectangle) 'width) value))

(defun rectangle-width (rect)
  (cffi:foreign-slot-value rect '(:struct rectangle) 'width))

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-width)
      "Accessor"
      (documentation 'rectangle-width 'function)
 "@version{2024-3-1}
  @syntax{(pango:rectangle-width instance) => width}
  @syntax{(setf (pango:rectangle-width instance) width)}
  @argument[instance]{a @class{pango:rectangle} instance}
  @argument[width]{an integer with the width component of the rectangle}
  @begin{short}
    Accessor of the @code{width} slot of the @class{pango:rectangle} structure.
  @end{short}
  @see-class{pango:rectangle}")

(export 'rectangle-width)

;;; --- pango:rectangle-height -------------------------------------------------

(defun (setf rectangle-height) (value rect)
  (setf (cffi:foreign-slot-value rect '(:struct rectangle) 'height) value))

(defun rectangle-height (rect)
  (cffi:foreign-slot-value rect '(:struct rectangle) 'height))

#+liber-documentation
(setf (liber:alias-for-function 'rectangle-height)
      "Accessor"
      (documentation 'rectangle-height 'function)
 "@version{2024-3-1}
  @syntax{(pango:rectangle-height instance) => heigth}
  @syntax{(setf (pango:rectangle-height instance) height)}
  @argument[instance]{a @class{pango:rectangle} instance}
  @argument[height]{an integer with the height component of the rectangle}
  @begin{short}
    Accessor of the @code{height} slot of the @class{pango:rectangle} structure.
  @end{short}
  @see-class{pango:rectangle}")

(export 'rectangle-height)

;;; --- pango:rectangle-to-float -----------------------------------------------

(defun rectangle-to-float (rect)
  (values (rectangle-x rect)
          (rectangle-y rect)
          (rectangle-width rect)
          (rectangle-height rect)))

(export 'rectangle-to-float)

;;; ----------------------------------------------------------------------------

(defmacro with-rectangle ((var &rest args) &body body)
 #+liber-documentation
 "@version{2024-2-29}
  @syntax{(pango:with-rectangle (rect) body) => result}
  @syntax{(pango:with-rectangle (rect x y width height) body) => result}
  @argument[rect]{a @symbol{pango:rectangle} instance to create and initialize}
  @argument[x]{an integer with the x coordinate of the rectangle}
  @argument[y]{an integer with the y coordinate of the rectangle}
  @argument[width]{an integer with the width of the rectangle}
  @argument[height]{an integer with the height of the rectangle}
  @begin{short}
    The @fun{pango:with-rectangle} macro allocates a new
    @symbol{pango:rectangle} instance, initializes the rectangle with the given
    values and executes the body that uses the rectangle.
  @end{short}
  After execution of the body the allocated memory for the rectangle is
  released.

  When no argument is given the components of the rectangle are initialized to
  zero. The initialization with four integer values initializes the @arg{x},
  @arg{y}, @arg{width}, and @arg{height} slots with the given values.
  @see-symbol{pango:rectangle}
  @see-macro{pango:with-rectangles}"
  (cond ((null args)
         ;; We have no arguments, the default is initialization with zeros.
         `(cffi:with-foreign-object (,var '(:struct rectangle))
            (cffi:with-foreign-slots ((x y width height)
                                      ,var
                                      (:struct rectangle))
              (setf x 0 y 0 width 0 height 0))
              (progn ,@body)))

        ((null (fifth args))
         ;; We have a list of four arguments with x, y, width, height values
         (destructuring-bind (x1 y1 width1 height1) args
           `(cffi:with-foreign-object (,var '(:struct rectangle))
              (cffi:with-foreign-slots ((x y width height)
                                        ,var
                                        (:struct rectangle))
                (setf x ,x1 y ,y1 width ,width1 height ,height1)
                (progn ,@body)))))
        (t
         (error "Syntax error in PANGO:WITH-RECTANGLE"))))

(export 'with-rectangle)

;;; ----------------------------------------------------------------------------

(defmacro with-rectangles (vars &body body)
 #+liber-documentation
 "@version{2024-2-29}
  @syntax{(pango:with-rectangles (rect1 rect2 ... rectn) body) => result}
  @argument[rect1 ... rectn]{the newly created @symbol{pango:rectangle}
    instances}
  @argument[body]{a body that uses the bindings @arg{rect1 ... rectn}}
  @begin{short}
    The @fun{pango:with-rectangles} macro creates new variable bindings and
    executes the body that use these bindings.
  @end{short}
  The macro performs the bindings sequentially, like the @sym{let*} macro.

  Each rectangle can be initialized with values using the syntax for the
  @fun{pango:with-rectangle} macro.
  @see-symbol{pango:rectangle}
  @see-macro{pango:with-rectangle}"
  (if vars
      (let ((var (if (listp (first vars)) (first vars) (list (first vars)))))
        `(with-rectangle ,var
           (with-rectangles ,(rest vars)
             ,@body)))
      `(progn ,@body)))

(export 'with-rectangles)

;;; ----------------------------------------------------------------------------
;;; struct PangoMatrix
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-cstruct matrix "PangoMatrix"
  (:export t
   :type-initializer "pango_matrix_get_type")
  (xx :double :initform 0.0d0)
  (xy :double :initform 0.0d0)
  (yx :double :initform 0.0d0)
  (yy :double :initform 0.0d0)
  (x0 :double :initform 0.0d0)
  (y0 :double :initform 0.0d0))

#+liber-documentation
(setf (liber:alias-for-class 'matrix)
      "GBoxed"
      (documentation 'matrix 'type)
 "@version{2024-3-1}
  @begin{short}
    A structure specifying a transformation between user-space coordinates and
    device coordinates.
  @end{short}
  The transformation is given by
  @begin{pre}
xdevice = xuser * xx + yuser * xy + x0
ydevice = xuser * yx + yuser * yy + y0
  @end{pre}
  @begin{pre}
(glib:define-gboxed-cstruct matrix \"PangoMatrix\"
  (:export t
   :type-initializer \"pango_matrix_get_type\")
  (xx :double :initform 0.0d0)
  (xy :double :initform 0.0d0)
  (yx :double :initform 0.0d0)
  (yy :double :initform 0.0d0)
  (x0 :double :initform 0.0d0)
  (y0 :double :initform 0.0d0))
  @end{pre}
  @begin[code]{table}
    @entry[xx]{1st component of the transformation matrix.}
    @entry[xy]{2nd component of the transformation matrix.}
    @entry[yx]{3rd component of the transformation matrix.}
    @entry[yy]{4th component of the transformation matrix.}
    @entry[x0]{x translation.}
    @entry[y0]{y translation.}
  @end{table}
  @see-constructor{pango:matrix-init}
  @see-constructor{pango:matrix-new}")

;;; --- pango:matrix-xx --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'matrix-xx)
      "Accessor"
      (documentation 'matrix-xx 'function)
 "@version{2024-3-1}
  @syntax{(pango:matrix-xx instance) => xx}
  @syntax{(setf (pango:matrix-xx instance) xx)}
  @argument[instance]{a @class{pango:matrix} instance}
  @argument[xx]{an double float with the xx component of the transformation
    matrix}
  @begin{short}
    Accessor of the @code{xx} slot of the @class{pango:matrix} structure.
  @end{short}
  @see-class{pango:matrix}")

;;; --- pango:matrix-xy --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'matrix-xy)
      "Accessor"
      (documentation 'matrix-xy 'function)
 "@version{2024-3-1}
  @syntax{(pango:matrix-xy instance) => xy}
  @syntax{(setf (pango:matrix-xy instance) xy)}
  @argument[instance]{a @class{pango:matrix} instance}
  @argument[xy]{an double float with the xy component of the transformation
    matrix}
  @begin{short}
    Accessor of the @code{xy} slot of the @class{pango:matrix} structure.
  @end{short}
  @see-class{pango:matrix}")

;;; --- pango:matrix-yx --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'matrix-yx)
      "Accessor"
      (documentation 'matrix-yx 'function)
 "@version{2024-3-1}
  @syntax{(pango:matrix-yx instance) => yx}
  @syntax{(setf (pango:matrix-yx instance) yx)}
  @argument[instance]{a @class{pango:matrix} instance}
  @argument[yx]{an double float with the yx component of the transformation
    matrix}
  @begin{short}
    Accessor of the @code{yx} slot of the @class{pango:matrix} structure.
  @end{short}
  @see-class{pango:matrix}")

;;; --- pango:matrix-yy --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'matrix-yy)
      "Accessor"
      (documentation 'matrix-yy 'function)
 "@version{2024-3-1}
  @syntax{(pango:matrix-yy instance) => yy}
  @syntax{(setf (pango:matrix-yy instance) yy)}
  @argument[instance]{a @class{pango:matrix} instance}
  @argument[yy]{an double float with the yy component of the transformation
    matrix}
  @begin{short}
    Accessor of the @code{yy} slot of the @class{pango:matrix} structure.
  @end{short}
  @see-class{pango:matrix}")

;;; --- pango:matrix-x0 --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'matrix-x0)
      "Accessor"
      (documentation 'matrix-x0 'function)
 "@version{2024-3-1}
  @syntax{(pango:matrix-x0 instance) => x0}
  @syntax{(setf (pango:matrix-x0 instance) x0)}
  @argument[instance]{a @class{pango:matrix} instance}
  @argument[x0]{an double float with the x0 component of the transformation
    matrix}
  @begin{short}
    Accessor of the @code{x0} slot of the @class{pango:matrix} structure.
  @end{short}
  @see-class{pango:matrix}")

;;; --- pango:matrix-y0 --------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-function 'matrix-y0)
      "Accessor"
      (documentation 'matrix-y0 'function)
 "@version{2024-3-1}
  @syntax{(pango:matrix-y0 instance) => y0}
  @syntax{(setf (pango:matrix-y0 instance) y0)}
  @argument[instance]{a @class{pango:matrix} instance}
  @argument[y0]{an double float with the y0 component of the transformation
    matrix}
  @begin{short}
    Accessor of the @code{y0} slot of the @class{pango:matrix} structure.
  @end{short}
  @see-class{pango:matrix}")

;;; ----------------------------------------------------------------------------
;;; PANGO_MATRIX_INIT
;;; ----------------------------------------------------------------------------

(defun matrix-init ()
 #+liber-documentation
 "@version{2024-2-29}
  @return{The newly allocated @class{pango:matrix} initialized to the identiy
    transform.}
  @begin{short}
    Constant that can be used to initialize a Pango matrix to the identity
    transform.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(let ((matrix (pango:matrix-init)))
  (pango:matrix-rotate matrix 45.0d0)
  ... )
    @end{pre}
  @end{dictionary}
  @see-class{pango:matrix}"
  (make-matrix :xx 1.0d0 :yy 1.0d0))

(export 'matrix-init)

;;; ----------------------------------------------------------------------------
;;; pango:matrix-new
;;; ----------------------------------------------------------------------------

(defun matrix-new (&key (xx 0.0d0) (xy 0.0d0) (yx 0.0d0) (yy 0.0d0)
                        (x0 0.0d0) (y0 0.0d0))
 #+liber-documentation
 "@version{2024-2-29}
  @argument[xx, xy, yx, yy, x0, y0]{numbers coerced to double floats with the
    components of the transformation matrix}
  @return{The newly allocated @class{pango:matrix} instance.}
  @short{Creates a new Pango matrix.}
  @see-class{pango:matrix}"
  (make-matrix :xx (coerce xx 'double-float)
               :xy (coerce xy 'double-float)
               :yx (coerce yx 'double-float)
               :yy (coerce yy 'double-float)
               :x0 (coerce x0 'double-float)
               :y0 (coerce y0 'double-float)))

(export 'matrix-new)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_copy ()
;;; ----------------------------------------------------------------------------

(defun matrix-copy (matrix)
 #+liber-documentation
 "@version{2024-2-29}
  @argument[matrix]{a @class{pango:matrix} instance}
  @return{The newly allocated @class{pango:matrix} instance.}
  @short{Copies a Pango matrix.}
  @see-class{pango:matrix}"
  (copy-matrix matrix))

(export 'matrix-copy)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_free ()
;;;
;;; void pango_matrix_free (PangoMatrix *matrix);
;;;
;;; Free a PangoMatrix created with pango_matrix_copy().
;;;
;;; matrix :
;;;     a PangoMatrix, may be NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango:matrix-to-float
;;; ----------------------------------------------------------------------------

(defun matrix-to-float (matrix)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @return{The list with the floating point values.}
  @short{Converts the matrix to al list of floating point values.}
  @note{This function is a Lisp extension and not present in the C library.}
  @see-class{pango:matrix}"
  (list (matrix-xx matrix)
        (matrix-xy matrix)
        (matrix-yx matrix)
        (matrix-yy matrix)
        (matrix-x0 matrix)
        (matrix-y0 matrix)))

(export 'matrix-to-float)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_translate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_translate" %matrix-translate) :void
  (matrix (g:boxed matrix))
  (tx :double)
  (ty :double))

(defun matrix-translate (matrix tx ty)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @argument[tx]{a number coerced to a double float with the amount to translate
    in the x direction}
  @argument[ty]{a number coerced to a double float with the amount to translate
    in the Y direction}
  @return{The @class{pango:matrix} instance with the transformation.}
  @begin{short}
    Changes the transformation represented by @arg{matrix} to be the
    transformation given by first translating by (@arg{tx}, @arg{ty}) then
    applying the original transformation.
  @end{short}
  @see-class{pango:matrix}"
  (%matrix-translate matrix
                     (coerce tx 'double-float)
                     (coerce ty 'double-float))
  matrix)

(export 'matrix-translate)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_scale ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_scale" %matrix-scale) :void
  (matrix (g:boxed matrix))
  (sx :double)
  (sy :double))

(defun matrix-scale (matrix sx sy)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @argument[sx]{a number coerced to a double float with the amount to scale by
    in x direction}
  @argument[sy]{a number coerced to a double float with the amount to scale by
    in y direction}
  @return{The @class{pango:matrix} instance with the transformation.}
  @begin{short}
    Changes the transformation represented by @arg{matrix} to be the
    transformation given by first scaling by @arg{sx} in the x direction and
    @arg{sy} in the y direction then applying the original transformation.
  @end{short}
  @see-class{pango:matrix}"
  (%matrix-scale matrix
                 (coerce sx 'double-float)
                 (coerce sy 'double-float))
  matrix)

(export 'matrix-scale)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_rotate ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_rotate" %matrix-rotate) :void
  (matrix (g:boxed matrix))
  (degrees :double))

(defun matrix-rotate (matrix degrees)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @argument[degrees]{a number coerced to a double float with the degrees to
    rotate counter-clockwise}
  @return{The @class{pango:matrix} instance with the transformation.}
  @begin{short}
    Changes the transformation represented by @arg{matrix} to be the
    transformation given by first rotating by @arg{degrees} degrees
    counter-clockwise then applying the original transformation.
  @end{short}
  @see-class{pango:matrix}"
  (%matrix-rotate matrix (coerce degrees 'double-float))
  matrix)

(export 'matrix-rotate)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_concat ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_concat" %matrix-concat) :void
  (matrix (g:boxed matrix))
  (other (g:boxed matrix)))

(defun matrix-concat (matrix other)
 #+liber-documentation
 "@version{2024-1-3}
  @argument[matrix]{a @class{pango:matrix} instance}
  @argument[other]{a @class{pango:matrix} instance}
  @begin{short}
    Changes the transformation represented by @arg{matrix} to be the
    transformation given by first applying transformation given by
    @arg{other} then applying the original transformation.
  @end{short}
  @see-class{pango:matrix}"
  (%matrix-concat matrix other)
  matrix)

(export 'matrix-concat)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_transform_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_transform_point" %matrix-transform-point) :void
  (matrix (g:boxed matrix))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun matrix-transform-point (matrix x y)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @argument[x]{a number coerced to a double float with the x position}
  @argument[y]{a number coerced to a double float with the y position}
  @return{The double float values for the transformed point @code{(x, y)}.}
  @begin{short}
    Transforms the point @code{(x, y)} by @arg{matrix}.
  @end{short}
  @see-class{pango:matrix}"
  (cffi:with-foreign-objects ((xout :double) (yout :double))
    (setf (cffi:mem-ref xout :double) (coerce x 'double-float)
          (cffi:mem-ref yout :double) (coerce y 'double-float))
    (%matrix-transform-point matrix xout yout)
    (values (cffi:mem-ref xout :double)
            (cffi:mem-ref yout :double))))

(export 'matrix-transform-point)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_transform_distance ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_transform_distance" %matrix-transform-distance)
    :void
  (matrix (g:boxed matrix))
  (dx (:pointer :double))
  (dy (:pointer :double)))

(defun matrix-transform-distance (matrix dx dy)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @argument[dx]{a number coerced to a double float with the x component}
  @argument[dy]{a number coerced to a double float with the y component}
  @return{The double float values for the transformed distance vector
    @code{(dx, dy)}.}
  @begin{short}
    Transforms the distance vector @code{(dx,dy)} by @arg{matrix}.
  @end{short}
  This is similar to the @fun{pango:matrix-transform-point} function except
  that the translation components of the transformation are ignored. The
  calculation of the returned vector is as follows:
  @begin{pre}
dx2 = dx1 * xx + dy1 * xy;
dy2 = dx1 * yx + dy1 * yy;
  @end{pre}
  Affine transformations are position invariant, so the same vector always
  transforms to the same vector. If @code{(x1, y1)} transforms to
  @code{(x2,y2)} then @code{(x1+dx1, y1+dy1)} will transform to
  @code{(x1+dx2, y1+dy2)} for all values of @code{x1} and @code{x2}.
  @see-class{pango:matrix}
  @see-function{pango:matrix-transform-point}"
  (cffi:with-foreign-objects ((dxout :double) (dyout :double))
    (setf (cffi:mem-ref dxout :double) (coerce dx 'double-float)
          (cffi:mem-ref dyout :double) (coerce dy 'double-float))
    (%matrix-transform-distance matrix dxout dyout)
    (values (cffi:mem-ref dxout :double)
            (cffi:mem-ref dyout :double))))

(export 'matrix-transform-distance)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_transform_rectangle ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_transform_rectangle" %matrix-transform-rectangle)
    :void
  (matrix (g:boxed matrix))
  (rect (:pointer (:struct rectangle))))

(defun matrix-transform-rectangle (matrix rect)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @argument[rect]{a @class{pango:rectangle} instance}
  @return{The transformed @class{pango:rectangle} instance with the bounding
    box.}
  @begin{short}
    First transforms @arg{rect} using @arg{matrix}, then calculates the
    bounding box of the transformed rectangle.
  @end{short}
  The rectangle should be in Pango units. This function is useful for example
  when you want to draw a rotated @class{pango:layout} object to an image
  buffer, and want to know how large the image should be and how much you should
  shift the layout when rendering. If you have a rectangle in device units
  (pixels), use the @fun{pango:matrix-transform-pixel-rectangle} function.

  If you have the rectangle in Pango units and want to convert to transformed
  pixel bounding box, it is more accurate to transform it first using this
  function and pass the result to the @fun{pango:extents-to-pixels} function,
  first argument, for an inclusive rounded rectangle. However, there are valid
  reasons that you may want to convert to pixels first and then transform, for
  example when the transformed coordinates may overflow in Pango units, large
  matrix translation for example.
  @see-class{pango:matrix}
  @see-symbol{pango:rectangle}
  @see-class{pango:layout}
  @see-function{pango:matrix-transform-pixel-rectangle}
  @see-function{pango:extents-to-pixels}"
  (%matrix-transform-rectangle matrix rect)
  rect)

(export 'matrix-transform-rectangle)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_transform_pixel_rectangle ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_transform_pixel_rectangle"
               %matrix-transform-pixel-rectangle) :void
  (matrix (g:boxed matrix))
  (rect (:pointer (:struct rectangle))))

(defun matrix-transform-pixel-rectangle (matrix rect)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @argument[rect]{a @class{pango:rectangle} instance}
  @return{The transformed @class{pango:rectangle} instance with the bounding
    box.}
  @begin{short}
    First transforms @arg{rect} using @arg{matrix}, then calculates the
    bounding box of the transformed rectangle.
  @end{short}
  The rectangle should be in device units (pixels). This function is useful for
  example when you want to draw a rotated @class{pango:layout} object to an
  image buffer, and want to know how large the image should be and how much you
  should shift the layout when rendering.

  For better accuracy, you should use the @fun{pango:matrix-transform-rectangle}
  function on original rectangle in Pango units and convert to pixels afterward
  using the @fun{pango:extents-to-pixels} functions first argument.
  @see-class{pango:matrix}
  @see-symbol{pango:rectangle}
  @see-class{pango:layout}
  @see-function{pango:matrix-transform-rectangle}
  @see-function{pango:extents-to-pixels}"
  (%matrix-transform-pixel-rectangle matrix rect)
  rect)

(export 'matrix-transform-pixel-rectangle)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_get_font_scale_factor ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_get_font_scale_factor" matrix-font-scale-factor)
    :double
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @return{The double float with the scale factor of @arg{matrix} on the height
    of the font, or 1.0 if @arg{matrix} is @code{nil}.}
  @begin{short}
    Returns the scale factor of a matrix on the height of the font.
  @end{short}
  That is, the scale factor in the direction perpendicular to the vector that
  the x coordinate is mapped to.
  @see-class{pango:matrix}"
  (matrix (g:boxed matrix)))

(export 'matrix-font-scale-factor)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_get_font_scale_factors ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_matrix_get_font_scale_factors" %matrix-font-scale-factors)
    :void
  (matrix (g:boxed matrix))
  (xscale (:pointer :double))
  (yscale (:pointer :double)))

(defun matrix-font-scale-factors (matrix)
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @return{The double float @arg{xscale}, @arg{yscale} values with the scale
    factor in the x direction and in the y direction.}
  @begin{short}
    Calculates the scale factor of a matrix on the width and height of the font.
  @end{short}
  That is, @arg{xscale} is the scale factor in the direction of the x
  coordinate, and @arg{yscale} is the scale factor in the direction
  perpendicular to the vector that the x coordinate is mapped to.

  Note that output numbers will always be non-negative.
  @see-class{pango:matrix}"
  (cffi:with-foreign-objects ((xscale :double) (yscale :double))
    (%matrix-font-scale-factors matrix xscale yscale)
    (values (cffi:mem-ref xscale :double)
            (cffi:mem-ref yscale :double))))

(export 'matrix-font-scale-factors)

;;; ----------------------------------------------------------------------------
;;; pango_matrix_get_slant_ratio
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_matrix_get_slant_ratio" matrix-slant-ratio) :double
 #+liber-documentation
 "@version{2024-3-1}
  @argument[matrix]{a @class{pango:matrix} instance}
  @return{The double float with the slant ration of @arg{matrix}.}
  @short{Gets the slant ratio of the matrix.}
  This is @code{λ} for a shear matrix in the form
  @begin{pre}
1 λ
0 1
  @end{pre}
  Since 1.50
  @see-class{pango:matrix}"
  (matrix (g:boxed matrix)))

#+pango-1-50
(export 'matrix-slant-ratio)

;;; --- End of file pango.matrix.lisp ------------------------------------------
