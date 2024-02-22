(in-package :pango-example)

(defun pango-png-image-draw (&optional (drawfunc #'pango-draw-text-centered))
  (let ((width 600) (height 300))
    (cairo:with-context-for-image-surface (context :argb32 width height)

      (funcall drawfunc context width height)
      (cairo:show-page context)

      ;; Create and save the PNG image
      (format t "Write image to ~a~%" (sys-path "out/png-draw.png"))
      (cairo:surface-write-to-png (cairo:target context)
                                  (sys-path "out/png-draw.png")))))

;;; --- 2024-1-13 --------------------------------------------------------------
