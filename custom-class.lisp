(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))

(defpackage :custom-class
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :custom-class)



;; http://scentric.net/tutorial/sec-custom-cell-renderers.html
#+nil
(defcallback custom-cell-renderer-progress-finalize :void ((obj (g-object g-object-class)))
  (let* ((parent (foreign-slot-value obj (g-object g-object-class)))
	 (fin (foreign-slot-value parent-class (g-object g-object-class) :finalize)))
    (foreign-funcall fin :void (obj (g-object g-object-class)))))

#+nil
(defcallback custom-cell-renderer-progress-init :void ((cell :pointer))
  ;; CustomCellRendererProgress* CELL 
  (setf (gtk-cell-renderer-mode cell)))
#+nil
(defcallback custom-cell-renderer-progress-class-init (class)
  ;; GTK_CELL_RENDERER_CLASS
  (let ((parent-class (g-type-class-peek-parent class) :parent-class))
    (setf (foreign-slot-value class (g-object g-object-class) :finalize) )))

(defcstruct _my-ip-address (entry (g-object gtk-entry)))
(defcstruct _my-ip-address-class (parent-class (g-object gtk-entry-c)))
(defcallback my-ip-address-class-init :void ((klass :pointer) (data (g-object gpointer)))
  ;; REGISTER signals
  )
(defcallback my-ip-address-init :void ((ip-address :pointer))
  ;; REGISTER signals
  )
;;(foreign-type-size '_my-ip-address-class)
#+nil
(let ((entry-type 0))
  (defun my-ip-address-get-type ()
    (when (= 0 entry-type)
      (cffi:with-foreign-object (info '(:struct gobject:g-type-info))
	(setf (foreign-slot-value info :uint16 :class-size) (foreign-type-size '_my-ip-address-class)
	      (foreign-slot-value info :pointer :class-init-fn) my-ip-address-class-init
	      (foreign-slot-value info :pointer :instance-init-fn) my-ip-address-init)
	      )
	(g-type-register-static (g-type-from-name "GtkEntry")
				"MyIPAddress"
				info
				0)))
    entry-type))

;; (g-object-new (my-ip-address-get-type)
;;(foreign-slot-offset )


#+nil
(cffi:with-foreign-object (info '(:struct gobject:g-type-info))
  (let ((cell-progress-type 0)
       (custom-class nil))
    (defun custom-cell-renderer-progress-get-type ()
     (when (= 0 cell-progress-type)
       (setf
	custom-class (cffi:defcstruct custom_class
		       (parent-class (g-object gtk-cell-renderer-class)))
	(foreign-slot-value info :uint16 :class-size) (foreign-type-size custom-class)
	(foreign-slot-value info :pointer :class-init-fn) 
	cell-progress-type
	(g-type-register-static (cffi:foreign-funcall "gtk_cell_renderer_get_type" gobject::g-type)
				"CustomCellRenderProgress"
				info 0)))
     cell-progress-type)))
#+nil
(gobject:g-object  'gtk-entry)

#+nil
(g-type-from-name "GtkEntry")
#+inl
(defparameter *bla* (cffi:defcstruct custom_class (parent-class (g-object gtk-cell-renderer-class))))
#+nil
(foreign-type-size *bla*)

#+nil
(foreign-type-size '(:struct gobject:g-type-info))
#+nil
(stable-sort (foreign-slot-names '(:struct gobject:g-type-info)) #'string-lessp) 

#+nil
(sb-int:with-float-traps-masked (:divide-by-zero)
 (defparameter *bla* (make-instance 'gtk-cell-renderer)))
