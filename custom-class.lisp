#+nil (eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))

#+nil
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

(defcstruct _my-ip-address (entry (:struct %gtk-entry)))
(defcstruct _my-ip-address-class (parent-class (:struct %gtk-entry-class)))
(defcallback my-ip-address-class-init :void ((klass :pointer) (data (g-object gpointer)))
  ;; REGISTER signals
  (format t "~A~%" 'class-init)
  )
(defcallback my-ip-address-init :void ((ip-address :pointer))
  ;; REGISTER signals
  (format t "~A~%" 'init)
  )
#+nil
(defparameter *bla* (g-type-from-name "GtkEntry"))



#+nil
(foreign-type-size '_my-ip-address-class)
(let ((entry-type 0))
  (defun my-ip-address-get-type ()
    (when (= 0 entry-type)
      (cffi:with-foreign-object
       (info '(:struct gobject:g-type-info))
       (setf (foreign-slot-value info '(:struct gobject:g-type-info) :class-size)
	     (foreign-type-size '_my-ip-address-class)
	     (foreign-slot-value info '(:struct gobject:g-type-info) :instance-size)
	     (foreign-type-size '_my-ip-address)
	     (foreign-slot-value info '(:struct gobject:g-type-info) :class-init-fn)
	     (callback my-ip-address-class-init)
	     (foreign-slot-value info '(:struct gobject:g-type-info) :instance-init-fn)
	     (callback my-ip-address-init)
	     entry-type (g-type-register-static (g-type-from-name "GtkEntry")
						"MyIPAddress"
						info
						0))))
    entry-type)
  (defun get-entry-type ()
    entry-type))
#+nil
(g-type-from-name "GtkEntry")

#+nil
(gobject::%g-type-from-name "GtkEntry")
#+nil
(foreign-type-size '_my-ip-address-class)
#+nil
(foreign-type-size '_my-ip-address)
#+nil
(foreign-type-size custom-class-internal: %gtk-entry)
#+nil
(cffi:foreign-type-size '(:struct %gtk-cell-renderer))
#+nil
(g-type-register-static-simple (gobject::%g-type-from-name "GtkEntry")
			       "MyIPAddress"
			       984
			       (callback my-ip-address-class-init)
			       40
			       (callback my-ip-address-init)
			       0)



(let ((entry-type 0))
  (defun my-ip-address-get-type-simple ()
    (when (= 0 entry-type)
      (setf entry-type
	    (g-type-register-static-simple (g-type-from-name "GtkEntry")
					   "MyIPAddress"
					   (foreign-type-size '_my-ip-address-class)
					   (callback my-ip-address-class-init)
					   (foreign-type-size '_my-ip-address)
					   (callback my-ip-address-init)
					   0)))
    entry-type)
  (defun get-entry-type-simple ()
    entry-type))

#+nil
(cffi:with-foreign-object
 (info '(:struct gobject:g-type-info)
  )
 (foreign-slot-value info '(:struct gobject:g-type-info) :class-size)
 )

#+nil
(my-ip-address-get-type-simple)
#+nil
(get-entry-type-simple)
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
