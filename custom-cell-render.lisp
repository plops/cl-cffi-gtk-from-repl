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

(defcallback custom-cell-renderer-progress-get-property :void ((object :pointer)
							       (prop-id :uint)
							       (value :pointer)
							       (parameter-spec :pointer))
  (case prop-id
    (*prop-percentage* (g-value-set-double value
					   (slot-value (mem-ref object '(:struct _custom-cell-renderer-progress))
						       'progress)))
    (t (format t "invalid property id ~d." param-id))))

(defcallback custom-cell-renderer-progress-set-property :void ((object :pointer)
							       (prop-id :uint)
							       (value :pointer)
							       (parameter-spec :pointer))
  (case prop-id
    (*prop-percentage* (setf (slot-value (mem-ref object '(:struct _custom-cell-renderer-progress))
					 'progress)
			     (g-value-get-double value)))
    (t (format t "invalid property id ~d." param-id))))

(defun custom-cell-renderer-progress-new ()
  (g-object-newv "CustomCellRendererProgress" 0 (cffi:null-pointer)))


(defparameter *fixed-width* 100)
(defparameter *fixed-height* 10)

(defcallback custom-cell-renderer-progress-get-size :void ((cell :pointer)
							   (widget :pointer)
							   (cell-area :pointer)
							   (x-offset (:pointer :int))
							   (y-offset (:pointer :int))
							   (pwidth (:pointer :int))
							   (pheight (:pointer :int)))
  (with-foreign-slots ((xpad ypad xalign yalign width height)
		       (foreign-slot-value cell '(:struct %gtk-cell-renderer) 'priv)
		       (:struct %gtk-cell-renderer-private))
    (let ((w (+ *fixed-width* (* 2 xpad)))
	  (h (+ *fixed-height* (* 2 ypad))))
      (unless (null-pointer-p pwidth)
	(setf (mem-ref pwidth :int) w))
      (unless (null-pointer-p pheight)
	(setf (mem-ref pheight :int) h))
      (unless (null-pointer-p cell-area)
	(unless (null-pointer-p x-offset)
	  (setf (mem-ref x-offset :int) (max 0 (* xalign (- width w)))))
	(unless (null-pointer-p y-offset)
	  (setf (mem-ref y-offset :int) (max 0 (* yalign (- height h)))))))))

(defcallback custom-cell-renderer-progress-render :void ())


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


(let ((cell-progress-type nil))
  (defun custom-cell-renderer-progress-get-type-simple ()
    (or cell-progress-type
	(setf cell-progress-type
	      (g-type-register-static-simple
	       (g-type-from-name "GtkCellRenderer") "CustomCellRendererProgress"
	       (foreign-type-size '(:struct _custom-cell-renderer-progress-class))
	       (callback custom-cell-renderer-progress-class-init)
	       (foreign-type-size '(:struct _custom-cell-renderer-progress))
	       (callback custom-cell-renderer-progress-init)
	       0)))))
