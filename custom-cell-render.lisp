(in-package :custom-class)

;; http://scentric.net/tutorial/sec-custom-cell-renderers.html


(defcstruct _custom-cell-renderer-progress
  (parent (:struct %gtk-cell-renderer)))
(defcstruct _custom-cell-renderer-progress-class
  (parent (:struct %gtk-cell-renderer-class)))

(defcallback custom-cell-renderer-progress-init :void (renderer :pointer)
  (with-foreign-slots ((mode xpad ypad) renderer '(:struct %gtk-cell-renderer))))

(let ((cell-progress-type nil))
  (defun custom-cell-renderer-progress-get-type-simple ()
    (or cell-progress-type
	(setf cell-progress-type
	      (g-type-register-static-simple (g-type-from-name "GtkCellRenderer") "CustomCellRenderer"
					     (foreign-type-size '(:struct _custom-cell-renderer-progress-class))
					     (callback custom-cell-renderer-progress-class-init)
					     (foreign-type-size '(:struct _custom-cell-renderer-progress))
					     (callback custom-cell-renderer-progress-init)
					     0)))))
