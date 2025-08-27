;;; ----------------------------------------------------------------------------
;;; pango.tab-array.lisp
;;;
;;; The documentation in this file is taken from the Pango Reference Manual
;;; version 1.56 and modified to document the Lisp binding to the Pango
;;; library, see <http://www.gtk.org>. The API documentation for the Lisp
;;; binding is available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; Tab Stops
;;;
;;;     Structures for storing tab stops
;;;
;;; Types and Values
;;;
;;;     PangoTabArray
;;;     PangoTabAlign
;;;
;;; Functions
;;;
;;;     pango_tab_array_new
;;;     pango_tab_array_new_with_positions
;;;     pango_tab_array_copy
;;;     pango_tab_array_free
;;;     pango_tab_array_get_size
;;;     pango_tab_array_resize
;;;
;;;     pango_tab_array_get_tab
;;;     pango_tab_array_set_tab
;;;     pango_tab_array_get_tabs
;;;
;;;     pango_tab_array_get_positions_in_pixels
;;;     pango_tab_array_set_positions_in_pixels             Since 1.50
;;;
;;;     pango_tab_array_get_decimal_point                   Since 1.50
;;;     pango_tab_array_set_decimal_point                   Since 1.50
;;;     pango_tab_array_sort                                Since 1.50
;;;     pango_tab_array_from_string                         Since 1.50
;;;     pango_tab_array_to_string                           Since 1.50
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── PangoTabArray
;;;
;;;     GEnum
;;;     ╰── PangoTabAlign
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; PangoTabAlign
;;; ----------------------------------------------------------------------------

(gobject:define-genum "PangoTabAlign" tab-align
  (:export t
   :type-initializer "pango_tab_align_get_type")
  (:left 0)
  #+pango-1-50
  (:right 1)
  #+pango-1-50
  (:center 2)
  #+pango-1-50
  (:decimal 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'tab-align)
      "GEnum"
      (liber:symbol-documentation 'tab-align)
 "@version{2025-08-24}
  @begin{declaration}
(gobject:define-genum \"PangoTabAlign\" tab-align
  (:export t
   :type-initializer \"pango_tab_align_get_type\")
  (:left 0)
  (:right 1)
  (:center 2)
  (:decimal 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:left]{The text appears to the right of the tab stop position.}
      @entry[:right]{The text appears to the left of the tab stop position
        until the available space is filled. Since 1.50}
      @entry[:center]{The text is centered at the tab stop position until the
        available space is filled. Since 1.50}
      @entry[:decimal]{Text before the first occurrence of the decimal point
        character appears to the left of the tab stop position, until the
        available space is filled, the rest to the right. Since 1.50}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{pango:tab-align} enumeration specifies where the text appears
    relative to the tab stop position.
  @end{short}
  @see-class{pango:tab-array}")

;;; ----------------------------------------------------------------------------
;;; PangoTabArray
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_new" %tab-array-new) :pointer
  (initial-size :int)
  (positions-in-pixels :boolean))

(glib:define-gboxed-opaque tab-array "PangoTabArray"
  :export t
  :type-initializer "pango_tab_array_get_type"
  :alloc (%tab-array-new 0 nil))

#+liber-documentation
(setf (liber:alias-for-class 'tab-array)
      "GBoxed"
      (documentation 'tab-array 'type)
 "@version{2024-02-24}
  @begin{declaration}
(glib:define-gboxed-opaque tab-array \"PangoTabArray\"
  :export t
  :type-initializer \"pango_tab_array_get_type\"
  :alloc (%tab-array-new 0 nil))
  @end{declaration}
  @begin{short}
    The @class{pango:tab-array} structure contains an array of tab stops.
  @end{short}
  The @class{pango:tab-array} structure is opaque, and has no user visible
  fields. It can be used to set tab stops in a @class{pango:layout} object.
  Each tab  stop has an alignment, a position, and optionally a character to
  use as decimal point.
  @see-constructor{pango:tab-array-new}
  @see-constructor{pango:tab-array-new-with-positions}
  @see-constructor{pango:tab-array-from-string}
  @see-constructor{pango:tab-array-copy}
  @see-class{pango:layout}")

(export 'tab-array)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_new" tab-array-new) (g:boxed tab-array :return)
 #+liber-documentation
 "@version{2024-02-24}
  @argument[size]{an integer for the initial number of tab stops to allocate,
    can be 0}
  @argument[positions-in-pixels]{a boolean whether positions are in pixel units}
  @return{The newly allocated @class{pango:tab-array} instance.}
  @begin{short}
    Creates an array of @arg{size} tab stops.
  @end{short}
  Tab stops are specified in pixel units if @arg{positions-in-pixels} is
  @em{true}, otherwise in Pango units. All stops are initially at position 0.
  @see-class{pango:tab-array}"
  (size :int)
  (positions-in-pixel :boolean))

(export 'tab-array-new)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_new_with_positions
;;; ----------------------------------------------------------------------------

(defun tab-array-new-with-positions (size positions-in-pixel &rest args)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[size]{an integer for the initial number of tab stops to allocate,
    can be 0}
  @argument[positions-in-pixels]{a boolean whether positions are in pixel units}
  @argument[args]{a list of pairs with the @sym{pango:tab-align} value and
    position of the tab stops}
  @return{The newly allocated @class{pango:tab-array} instance.}
  @begin{short}
    This is a convenience function that creates a @class{pango:tab-array}
    instance and allows you to specify the alignment and position of each tab
    stop.
  @end{short}
  You must provide an alignment and position for @arg{size} tab stops.
  @see-class{pango:tab-array}
  @see-symbol{pango:tab-align}"
  (let ((tabs (tab-array-new size positions-in-pixel)))
    (iter (for (align pos) on args by #'cddr)
          (for index from 0)
          (setf (tab-array-tab tabs index) (list align pos)))
    tabs))

(export 'tab-array-new-with-positions)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_copy" tab-array-copy)
    (g:boxed tab-array :return)
 #+liber-documentation
 "@version{2024-02-24}
  @argument[tabs]{a @class{pango:tab-array} instance to copy}
  @return{The newly allocated @class{pango:tab-array} instance.}
  @short{Copies a tab array.}
  @see-class{pango:tab-array}"
  (tabs (g:boxed tab-array)))

(export 'tab-array-copy)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_free
;;;
;;; Frees a tab array and associated resources.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_get_size" tab-array-size) :int
 #+liber-documentation
 "@version{2025-08-24}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @return{The integer for the number of tabs in @arg{tabs}.}
  @short{Gets the number of tab stops in the tab array.}
  @see-class{pango:tab-array}"
  (tabs (g:boxed tab-array)))

(export 'tab-array-size)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_resize
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_resize" tab-array-resize) :void
 #+liber-documentation
 "@version{2024-02-24}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @argument[size]{an integer for the new size of @arg{tabs}}
  @begin{short}
    Resizes a tab array.
  @end{short}
  You must subsequently initialize any tab stops that were added as a result
  of growing the tab array.
  @see-class{pango:tab-array}"
  (tabs (g:boxed tab-array))
  (size :int))

(export 'tab-array-resize)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_tab
;;; pango_tab_array_set_tab
;;; ----------------------------------------------------------------------------

(defun (setf tab-array-tab) (value tabs index)
  (destructuring-bind (align pos) value
    (cffi:foreign-funcall "pango_tab_array_set_tab"
                          (g:boxed tab-array) tabs
                          :int index
                          tab-align align
                          :int pos
                          :void)
    (values align pos)))

(cffi:defcfun ("pango_tab_array_get_tab" %tab-array-tab) :void
  (tabs (g:boxed tab-array))
  (index :int)
  (align (:pointer tab-align))
  (pos (:pointer :int)))

(defun tab-array-tab (tabs index)
 #+liber-documentation
 "@version{2025-08-24}
  @syntax{(pango:tab-array-tab tabs index} => align, pos
  @syntax{(setf (pango:tab-array-tab tabs index) (list align pos))}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @argument[index]{an integer for the index of the tab stop}
  @argument[align]{a @sym{pango:tab-align} value}
  @argument[pos]{an integer for the tab position}
  @begin{short}
    Gets or sets the alignment and position of a tab stop.
  @end{short}
  @see-class{pango:tab-array}
  @see-symbol{pango:tab-align}"
  (cffi:with-foreign-objects ((align 'tab-align) (pos :int))
    (%tab-array-tab tabs index align pos)
    (values (cffi:mem-ref align 'tab-align)
            (cffi:mem-ref pos :int))))

(export 'tab-array-tab)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_tabs
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_get_tabs" %tab-array-tabs) :void
  (tabs (g:boxed tab-array))
  (align :pointer)
  (pos :pointer))

(defun tab-array-tabs (tabs)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @begin{return}
    The list of @sym{pango:tab-align} values and positions for the tab stops
    in the tab array.
  @end{return}
  @begin{short}
    Returns a list of alignment and positiongs of the tab stops in the tab
    array.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(setf tabs (pango:tab-array-from-string \"100 decimal:150 right:200\"))
=> #<PANGO:TAB-ARRAY {1003CAF943@}>
(pango:tab-array-tabs tabs)
=> ((:LEFT 100) (:DECIMAL 150) (:RIGHT 200))
    @end{pre}
  @end{dictionary}
  @see-class{pango:tab-array}
  @see-symbol{pango:tab-align}"
  (let ((size (tab-array-size tabs)))
    (cffi:with-foreign-objects ((align-ptr :pointer) (pos-ptr :pointer))
      (%tab-array-tabs tabs align-ptr pos-ptr)
      (let ((align-arr (cffi:mem-ref align-ptr :pointer))
            (pos-arr (cffi:mem-ref pos-ptr :pointer)))
        (unwind-protect
          (iter (for i from 0 below size)
                (collect (list (cffi:mem-aref align-arr 'tab-align i)
                               (cffi:mem-aref pos-arr :int i))))
          (progn
            (glib:free align-arr)
            (glib:free pos-arr)))))))

(export 'tab-array-tabs)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_set_positions_in_pixels
;;; pango_tab_array_get_positions_in_pixels
;;; ----------------------------------------------------------------------------

#+pango-1-50
(defun (setf tab-array-positions-in-pixels) (value tabs)
  (cffi:foreign-funcall "pango_tab_array_set_positions_in_pixels"
                        (g:boxed tab-array) tabs
                        :boolean value
                        :void)
  value)

(cffi:defcfun ("pango_tab_array_get_positions_in_pixels"
               tab-array-positions-in-pixels) :boolean
 #+liber-documentation
 "@version{2024-02-24}
  @syntax{(pango:tab-array-positions-in-pixels tabs) => setting}
  @syntax{(setf (pango:tab-array-positions-in-pixels tabs) setting)}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @argument[setting]{a boolean whether positions ar in pixels}
  @begin{short}
    The @fun{pango:tab-array-positions-in-pixels} function returns @em{true} if
    the tab positions are in pixels, @em{false} if they are in Pango units.
  @end{short}
  The @setf{pango:tab-array-positions-in-pixels} function sets whether positions
  in the tab array are specified in pixels.
  @begin[Note]{dictionary}
    The @setf{pango:tab-array-positions-in-pixels} function is availabe since
    Pango version 1.50.
  @end{dictionary}
  @see-class{pango:tab-array}"
  (tabs (g:boxed tab-array)))

(export 'tab-array-positions-in-pixels)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_decimal_point
;;; pango_tab_array_set_decimal_point
;;; ----------------------------------------------------------------------------

#+pango-1-50
(defun (setf tab-array-decimal-point) (value tabs index)
  (cffi:foreign-funcall "pango_tab_array_set_decimal_point"
                        (g:boxed tab-array) tabs
                        :int index
                        g:unichar value
                        :void)
  value)

#+pango-1-50
(cffi:defcfun ("pango_tab_array_get_decimal_point" tab-array-decimal-point)
    g:unichar
 #+liber-documentation
 "@version{2025-08-24}
  @syntax{(pango:tab-array-decimal-point tabs index) => char}
  @syntax{(setf (pango:tab-array-decimal-point tabs index) char)}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @argument[index]{an integer for the index of a tab stop}
  @argument[char]{a character for the decimal point to use}
  @begin{short}
   Gets or sets the Unicode character to use as decimal point.
  @end{short}
  This is only relevant for tabs with @val[pango:tab-align]{:decimal} alignment,
  which align content at the first occurrence of the decimal point character.
  The default value of 0 means that Pango will use the decimal point according
  to the current locale.

  Since 1.50
  @see-class{pango:tab-array}"
  (tabs (g:boxed tab-array))
  (index :int))

#+pango-1-50
(export 'tab-array-decimal-point)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_sort
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_tab_array_sort" tab-array-sort) :void
 #+liber-documentation
 "@version{2024-02-24}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @begin{short}
    Utility function to ensure that the tab stops are in increasing order.
  @end{short}

  Since 1.50
  @see-class{pango:tab-array}"
  (tabs (g:boxed tab-array)))

#+pango-1-50
(export 'tab-array-sort)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_from_string
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_tab_array_from_string" tab-array-from-string)
    (g:boxed tab-array :return)
 #+liber-documentation
 "@version{2025-08-24}
  @argument[text]{a string for the tab stops for the tab array}
  @return{The newly allocated @class{pango:tab-array} instance.}
  @begin{short}
    Deserializes a @class{pango:tab-array} instance from a string.
  @end{short}
  This is the counterpart to the @fun{pango:tab-array-to-string} function. See
  that function for details about the format.

  Since 1.50
  @see-class{pango:tab-array}
  @see-function{pango:tab-array-to-string}"
  (text :string))

#+pango-1-50
(export 'tab-array-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_to_string
;;; ----------------------------------------------------------------------------

#+pango-1-50
(cffi:defcfun ("pango_tab_array_to_string" tab-array-to-string) :string
 #+liber-documentation
 "@version{2025-08-24}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @return{The string for the tab stops of the tab array.}
  @begin{short}
    Serializes a @class{pango:tab-array} instance to a string.
  @end{short}
  No guarantees are made about the format of the string, it may change between
  Pango versions. The intended use of this function is testing and debugging.
  The format is not meant as a permanent storage format.

  Since 1.50
  @see-class{pango:tab-array}
  @see-function{pango:tab-array-from-string}"
  (tabs (g:boxed tab-array)))

#+pango-1-50
(export 'tab-array-to-string)

;;; --- End of file pango.tab-array.lisp ---------------------------------------
