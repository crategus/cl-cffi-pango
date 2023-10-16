;;; ----------------------------------------------------------------------------
;;; pango.tab-array.lisp
;;;
;;; The documentation of this file is taken from the Pango Reference Manual
;;; Version 1.50 and modified to document the Lisp binding to the Pango
;;; library. See <http://www.gtk.org>. The API documentation of the Lisp
;;; binding is available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2023 Dieter Kaiser
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
;;;     pango_tab_array_set_positions_in_pixels            Since 1.50
;;;
;;;     pango_tab_array_get_decimal_point                  Since 1.50
;;;     pango_tab_array_set_decimal_point                  Since 1.50
;;;     pango_tab_array_sort                               Since 1.50
;;;     pango_tab_array_from_string                        Since 1.50
;;;     pango_tab_array_to_string                          Since 1.50
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── PangoTabArray
;;;
;;;     GEnum
;;;     ╰── PangoTabAlign
;;;
;;; Description
;;;
;;;     Functions in this section are used to deal with PangoTabArray objects
;;;     that can be used to set tab stop positions in a PangoLayout.
;;; ----------------------------------------------------------------------------

(in-package :pango)

;;; ----------------------------------------------------------------------------
;;; enum PangoTabAlign
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "PangoTabAlign" tab-align
  (:export t
   :type-initializer "pango_tab_align_get_type")
  (:left 0)
  (:right 1)
  (:center 2)
  (:decimal 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'tab-align)
      "GEnum"
      (liber:symbol-documentation 'tab-align)
 "@version{2023-10-14}
  @begin{short}
    The @symbol{pango:tab-align} enumeration specifies where the text appears
    relative to the tab stop position.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"PangoTabAlign\" tab-align
  (:export t
   :type-initializer \"pango_tab_align_get_type\")
  (:left 0)
  (:right 1)
  (:center 2)
  (:decimal 3))
  @end{pre}
  @begin[code]{table}
    @entry[:left]{The text appears to the right of the tab stop position.}
    @entry[:right]{The text appears to the left of the tab stop position until
      the available space is filled. Since 1.50}
    @entry[:center]{The text is centered at the tab stop position until the
      available space is filled. Since 1.50}
    @entry[:decimal]{Text before the first occurrence of the decimal point
      character appears to the left of the tab stop position, until the
      available space is filled, the rest to the right. Since 1.50}
  @end{table}
  @see-class{pango:tab-array}")

;;; ----------------------------------------------------------------------------
;;; PangoTabArray
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_new" %tab-array-new) :pointer
  (initial-size :int)
  (positions-in-pixels :boolean))

(glib:define-g-boxed-opaque tab-array "PangoTabArray"
  :export t
  :type-initializer "pango_tab_array_get_type"
  :alloc (%tab-array-new 0 nil))

#+liber-documentation
(setf (liber:alias-for-class 'tab-array)
      "GBoxed"
      (documentation 'tab-array 'type)
 "@version{2023-10-14}
  @begin{short}
    A @class{pango:tab-array} structure contains an array of tab stops.
  @end{short}
  The @class{pango:tab-array} structure is opaque, and has no user visible
  fields. It can be used to set tab stops in a @class{pango:layout} object.
  Each tab  stop has an alignment, a position, and optionally a character to
  use as decimal point.
  @see-class{pango:layout}")

(export 'tab-array)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_new ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_new" tab-array-new) (g:boxed tab-array :return)
 #+liber-documentation
 "@version{2023-10-14}
  @argument[size]{an integer with the initial number of tab stops to allocate,
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
;;; pango_tab_array_new_with_positions ()
;;;
;;; PangoTabArray *
;;; pango_tab_array_new_with_positions (gint size,
;;;                                     gboolean positions_in_pixels,
;;;                                     PangoTabAlign first_alignment,
;;;                                     gint first_position,
;;;                                     ...);
;;;
;;; This is a convenience function that creates a PangoTabArray and allows you
;;; to specify the alignment and position of each tab stop. You must provide an
;;; alignment and position for size tab stops.
;;;
;;; size :
;;;     number of tab stops in the array
;;;
;;; positions_in_pixels :
;;;     whether positions are in pixel units
;;;
;;; first_alignment :
;;;     alignment of first tab stop
;;;
;;; first_position :
;;;     position of first tab stop
;;;
;;; ... :
;;;     additional alignment/position pairs
;;;
;;; Returns :
;;;     the newly allocated PangoTabArray, which should be freed with
;;;     pango_tab_array_free().
;;; ----------------------------------------------------------------------------

(defun tab-array-new-with-positions (size positions-in-pixel &rest args)
  (let ((tabs (tab-array-new size positions-in-pixel)))
    (iter (for (alignment position) on args by #'cddr)
          (for index from 0)
          (setf (tab-array-tab tabs index) (list alignment position)))
    tabs))

(export 'tab-array-new-with-positions)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_copy ()
;;;
;;; PangoTabArray * pango_tab_array_copy (PangoTabArray *src);
;;;
;;; Copies a PangoTabArray
;;;
;;; src :
;;;     PangoTabArray to copy
;;;
;;; Returns :
;;;     the newly allocated PangoTabArray, which should be freed with
;;;     pango_tab_array_free().
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_copy" tab-array-copy)
    (g:boxed tab-array :return)
  (tabs (g:boxed tab-array)))

(export 'tab-array-copy)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_free ()
;;;
;;; void pango_tab_array_free (PangoTabArray *tab_array);
;;;
;;; Frees a tab array and associated resources.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_size ()
;;;
;;; gint pango_tab_array_get_size (PangoTabArray *tab_array);
;;;
;;; Gets the number of tab stops in tab_array.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; Returns :
;;;     the number of tab stops in the array.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_get_size" pango-tab-array-size) :int
  (tabs (g:boxed tab-array)))

(export 'tab-array-size)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_resize ()
;;;
;;; void pango_tab_array_resize (PangoTabArray *tab_array, gint new_size);
;;;
;;; Resizes a tab array. You must subsequently initialize any tabs that were
;;; added as a result of growing the array.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; new_size :
;;;     new size of the array
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_resize" tab-array-resize) :void
  (tabs (g:boxed tab-array))
  (size :int))

(export 'tab-array-resize)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_set_tab ()
;;;
;;; void pango_tab_array_set_tab (PangoTabArray *tab_array,
;;;                               gint tab_index,
;;;                               PangoTabAlign alignment,
;;;                               gint location);
;;;
;;; Sets the alignment and location of a tab stop. alignment must always be
;;; PANGO_TAB_LEFT in the current implementation.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; tab_index :
;;;     the index of a tab stop
;;;
;;; alignment :
;;;     tab alignment
;;;
;;; location :
;;;     tab location in Pango units
;;; ----------------------------------------------------------------------------

(defun (setf tab-array-tab) (value tabs index)
  (destructuring-bind (alignment location) value
    (cffi:foreign-funcall "pango_tab_array_set_tab"
                          (g:boxed tab-array) tabs
                          :int index
                          tab-align alignment
                          :int location
                          :void)
    (values alignment location)))

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_tab ()
;;;
;;; void pango_tab_array_get_tab (PangoTabArray *tab_array,
;;;                               gint tab_index,
;;;                               PangoTabAlign *alignment,
;;;                               gint *location);
;;;
;;; Gets the alignment and position of a tab stop.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; tab_index :
;;;     tab stop index
;;;
;;; alignment :
;;;     location to store alignment, or NULL. [out][allow-none]
;;;
;;; location :
;;;     location to store tab position, or NULL. [out][allow-none]
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_get_tab" %tab-array-tab) :void
  (tabs (g:boxed tab-array))
  (index :int)
  (alignment (:pointer tab-align))
  (location (:pointer :int)))

(defun tab-array-tab (tabs index)
  (cffi:with-foreign-objects ((alignment 'tab-align) (location :int))
    (%tab-array-tab tabs index alignment location)
    (values (cffi:mem-ref alignment 'tab-align)
            (cffi:mem-ref location :int))))

(export 'tab-array-tab)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_tabs ()
;;;
;;; void pango_tab_array_get_tabs (PangoTabArray *tab_array,
;;;                                PangoTabAlign **alignments,
;;;                                gint **locations);
;;;
;;; If non-NULL, alignments and locations are filled with allocated arrays of
;;; length pango_tab_array_get_size(). You must free the returned array.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; alignments :
;;;     location to store an array of tab stop alignments, or NULL
;;;
;;; locations :
;;;     location to store an array of tab positions, or NULL
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_set_positions_in_pixels
;;;
;;; Sets whether positions in this array are specified in pixels.
;;;
;;; Since 1.50
;;; ----------------------------------------------------------------------------

(defun (setf tab-array-positions-in-pixels) (value tabs)
  (cffi:foreign-funcall "pango_tab_array_set_positions_in_pixels"
                        (g:boxed tab-array) tabs
                        :boolean value
                        :void)
  value)

(export 'tab-array-positions-in-pixels)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_positions_in_pixels ()
;;;
;;; gboolean pango_tab_array_get_positions_in_pixels (PangoTabArray *tab_array)
;;;
;;; Returns TRUE if the tab positions are in pixels, FALSE if they are in Pango
;;; units.
;;;
;;; tab_array :
;;;     a PangoTabArray
;;;
;;; Returns :
;;;     whether positions are in pixels.
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_get_positions_in_pixels"
               tab-array-positions-in-pixels) :boolean
  (tabs (g:boxed tab-array)))

(export 'tab-array-positions-in-pixels)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_set_decimal_point
;;;
;;; Sets the Unicode character to use as decimal point.
;;;
;;; Since: 1.50
;;; ----------------------------------------------------------------------------

(defun (setf tab-array-decimal-point) (value tabs index)
  (cffi:foreign-funcall "pango_tab_array_set_decimal_point"
                        (g:boxed tab-array) tabs
                        :int index
                        g:unichar value
                        :void)
  value)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_get_decimal_point
;;;
;;; Gets the Unicode character to use as decimal point.
;;;
;;; Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_get_decimal_point" tab-array-decimal-point)
    g:unichar
  (tabs (g:boxed tab-array))
  (index :int))

(export 'tab-array-decimal-point)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_sort
;;;
;;; Utility function to ensure that the tab stops are in increasing order.
;;;
;;; Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_sort" tab-array-sort) :void
  (tabs (g:boxed tab-array)))

(export 'tab-array-sort)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_from_string
;;;
;;; Deserializes a PangoTabArray from a string.
;;;
;;; Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_from_string" tag-array-from-string)
    (g:boxed tab-array :return)
  (text :string))

(export 'tab-array-from-string)

;;; ----------------------------------------------------------------------------
;;; pango_tab_array_to_string
;;;
;;; Serializes a PangoTabArray to a string.
;;;
;;; Since 1.50
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("pango_tab_array_to_string" tab-array-to-string) :string
  (tabs (g:boxed tab-array)))

(export 'tab-array-to-string)

;;; --- End of file pango.tab-array.lisp ---------------------------------------
