(in-package :custom-class)

;; http://scentric.net/tutorial/sec-custom-cell-renderers.html


(defcstruct _custom-cell-renderer-progress
  (parent (:struct %gtk-cell-renderer)))
(defcstruct _custom-cell-renderer-progress-class
  (parent (:struct %gtk-cell-renderer-class)))

(defcallback custom-cell-renderer-progress-init :void ((renderer :pointer))
  (with-foreign-slots ((xpad ypad)
		       (foreign-slot-value renderer '(:struct %gtk-cell-renderer) 'priv)
		       (:struct %gtk-cell-renderer-private))
    (setf xpad 2
	  ypad 2)))

(defparameter *parent-class* nil)

(defcallback custom-cell-renderer-progress-finalize :void ((renderer :pointer))
  (foreign-funcall-pointer (foreign-slot-value *parent-class*
					       '(:struct %gobject-class) 'finalize)
			   nil
			   :pointer renderer))



(defcallback custom-cell-renderer-progress-init-class :void ((klass :pointer))
  (setf *parent-class* (g-type-class-peek-parent klass))
  (setf (foreign-slot-value klass '(:struct %gobject-class) 'finalize)
	(callback custom-cell-renderer-progress-finalize)))

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
