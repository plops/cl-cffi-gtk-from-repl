(in-package :custom-class)

;; http://scentric.net/tutorial/sec-custom-cell-renderers.html


(defcstruct _custom-cell-renderer-progress
  (parent (:struct %gtk-cell-renderer)))
(defcstruct _custom-cell-renderer-progress-class
  (parent (:struct %gtk-cell-renderer-class)))

(defcallback custom-cell-renderer-progress-init :void ((renderer :pointer))
  (with-foreign-slots
      ((xpad ypad)
       (foreign-slot-value renderer '(:struct %gtk-cell-renderer) 'priv)
       (:struct %gtk-cell-renderer-private))
    (setf ;; fixme mode inert not accessible because it is in a bitmask
     xpad 2
     ypad 2)))

(defparameter *parent-class* nil)

(defcallback custom-cell-renderer-progress-finalize :void ((renderer :pointer))
  (foreign-funcall-pointer
   (foreign-slot-value *parent-class*
		       '(:struct %gobject-class) 'finalize)
   nil
   :pointer renderer))

(defparameter *prop-percentage* 1)

(defcallback custom-cell-renderer-progress-class-init :void ((klass :pointer))
  (setf *parent-class* (g-type-class-peek-parent klass))
  (with-foreign-slots ((finalize get-property set-property) klass (:struct %gobject-class))
    (setf finalize (callback custom-cell-renderer-progress-finalize)
	  get-property (callback custom-cell-renderer-progress-get-property)
	  set-property (callback custom-cell-renderer-progress-set-property)))

  (with-foreign-slots ((get-size render) klass (:struct %gtk-cell-renderer-class))
    (setf get-size (callback custom-cell-renderer-progress-get-size)
	  render (callback custom-cell-renderer-progress-render)))

  (g-object-class-install-property klass
				   *prop-percentage*
				   (g-param-spec-double (format nil "percentage")
							(format nil "Percentage")
							(format nil "The fractional progress to display.")
							0 1 0 '(:readable :writable))))

;; fixme implement get-prop set-prop get-size render 

(let ((cell-progress-type nil))
  (defun custom-cell-renderer-progress-get-type-simple ()
    (or cell-progress-type
	(setf cell-progress-type
	      (g-type-register-static-simple
	       (g-type-from-name "GtkCellRenderer") "CustomCellRenderer"
	       (foreign-type-size '(:struct _custom-cell-renderer-progress-class))
	       (callback custom-cell-renderer-progress-class-init)
	       (foreign-type-size '(:struct _custom-cell-renderer-progress))
	       (callback custom-cell-renderer-progress-init)
	       0)))))
