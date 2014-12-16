(in-package :custom-class)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; http://scentric.net/tutorial/sec-custom-cell-renderers.html


;; gtk-entry child in my-ip-address object is not a pointer but the
;; full structure. that allows to call its signals, gtk-entry is the
;; only child, private properties are stored in a different way using
;; GObject. if a widget doesn't need to react to changes of a
;; variable, it can be placed into this public widget structure.

;; each of the four numbers of the ip address is defined as a widget
;; property. the signal "ip-changed" is called when the address is
;; successfully changed
(defcstruct _my-ip-address (entry (:struct %gtk-entry)))
(defcstruct _my-ip-address-class
  (parent-class (:struct %gtk-entry-class))
  (ip-changed :pointer))
(defcstruct _my-ip-address-private (address :int :count 4))

(defparameter *prop-ip1* 1)
(defparameter *prop-ip2* 2)
(defparameter *prop-ip3* 3)
(defparameter *prop-ip4* 4)
(defcallback my-ip-address-set-property :void ((object :pointer)
					       (prop-id :unsigned-int)
					       (value :pointer)
					       (parameter-spec :pointer))
  (let ((address (make-array 4 :initial-element -1 :element-type 'fixnum)))
   (case prop-id
     (*prop-ip1* (setf (aref address 0) (g-value-get-int value))
		 (my-ip-address-set-address object address))
     (*prop-ip2* (setf (aref address 1) (g-value-get-int value))
		 (my-ip-address-set-address object address))
     (*prop-ip3* (setf (aref address 2) (g-value-get-int value))
		 (my-ip-address-set-address object address))
     (*prop-ip4* (setf (aref address 3) (g-value-get-int value))
		 (my-ip-address-set-address object address))
     (otherwise (break "invalid property id.")))))

(defparameter *changed-signal* 0)
(defparameter *my-ip-address-signal* (make-array 1 :element-type '(unsigned-byte 64)))

(defcfun ("g_type_instance_get_private" g-type-instance-get-private)
    :pointer
  (instance :pointer) ;; GTypeInstance
  (private-type g-type))

(defcallback my-ip-address-get-property :void ((object :pointer)
					       (prop-id :unsigned-int)
					       (value :pointer)
					       (parameter-spec :pointer))
  (let ((priv (g-type-instance-get-private object (my-ip-address-get-type-simple))))
    (case prop-id
      (*prop-ip1* (g-value-set-int
		   value
		   (mem-ref
		    (foreign-slot-value priv '(:struct _my-ip-address-private)
		     'address)
		    :int
		    0)))
      (*prop-ip2* (g-value-set-int
		   value
		   (mem-ref
		    (foreign-slot-value priv '(:struct _my-ip-address-private)
		     'address)
		    :int
		    1)))
      (*prop-ip3* (g-value-set-int
		   value
		   (mem-ref
		    (foreign-slot-value priv '(:struct _my-ip-address-private)
		     'address)
		    :int
		    2)))
      (*prop-ip4* (g-value-set-int
		   value
		   (mem-ref
		    (foreign-slot-value priv '(:struct _my-ip-address-private)
		     'address)
		    :int
		    3)))
      (otherwise (break "invalid property id.")))))

(defcfun ("g_signal_new" g-signal-new) :uint
  (signal-name :string)
  (itype g-type)
  (signal-flags g-signal-flags)
  (class-offset :uint)
  (accumulator :pointer)
  (accu-data :pointer)
  (marschaller :pointer)
  (return-type g-type)
  (n-params :uint)
  &rest)

(defcallback my-ip-address-class-init :void ((klass :pointer) (data (g-object gpointer)))
  (declare (ignore data))
  (defparameter *class-init* klass)
  (format t "~A~%" (list 'class-init))
  (setf (foreign-slot-value klass '(:struct %gobject-class) 'set-property) (callback my-ip-address-set-property)
	(foreign-slot-value klass '(:struct %gobject-class) 'get-property) (callback my-ip-address-get-property))
  (g-type-class-add-private klass (foreign-type-size '(:struct _my-ip-address-private)))
  (setf (aref *my-ip-address-signal* 0)
	(g-signal-new "ip-changed"
		      (g-type-from-class klass)
		      '(:run-first :action)
		      (foreign-slot-offset '(:struct _my-ip-address-class)
					   'ip-changed)
		      (null-pointer)
		      (null-pointer)
		      (foreign-symbol-pointer "g_cclosure_marshal_VOID__VOID")
		      +g-type-none+
		      0))
  (loop for (prop i str) in '((*prop-ip1* 1 "first")
			      (*prop-ip2* 2 "second")
			      (*prop-ip3* 3 "third")
			      (*prop-ip4* 4 "fourth"))
     do
       (g-object-class-install-property klass
					*prop-ip1*
					(g-param-spec-int (format nil "ip-number-~d" i)
							  (format nil "IP Address Number ~d" i)
							  (format nil "The ~s IP address number" str)
							  0 255 0 '(:readable :writable))))
  (format t "signal ip-changed has been created~%"))

(defcfun ("pango-font-description-free" pango-font-description-free) :void (description :pointer))


(defcallback my-ip-address-init :void ((ip-address :pointer))
  (let ((priv (g-type-instance-get-private ip-address (my-ip-address-get-type-simple))))
    (dotimes (i 4)
      (setf (mem-ref
		    (foreign-slot-value priv
					'(:struct _my-ip-address-private)
		     'address)
		    :int
		    i)
	    0))
    (let ((fd (pango-font-description-from-string "Monospace")))
      (gtk-widget-modify-font (g-type-check-instance-cast  ip-address
							   (gtk-widget-get-type))
			      fd)
      (my-ip-address-render ip-address)
      #+nil (pango-font-description-free fd))
    #+nil (g-signal-connect ip-address "key-press-event" (callback my-ip-address-key-pressed))
    #+nil (g-signal-connect ip-address "notify::cursor-position" (callback my-ip-address-move-cursor))))

(defcallback my-ip-address-move-cursor ((entry :pointer) ;; GObject
					      (spec (:pointer g-param-spec)))
  (let ((cursor (gtk-editable-get-position entry)))
    (cond ((<= cursor 3) (gtk-editable-set-position entry 3)))))


(defcallback my-ip-address-key-pressed :boolean ((entry :pointer) ; GObject
						 (event :pointer) ; GdkEventKey
						 )
  T)


(let ((entry-type nil))
  (defun my-ip-address-get-type-simple ()
    (unless entry-type
      (setf entry-type
	    (g-type-register-static-simple (g-type-from-name "GtkEntry")
					   "MyIPAddress"
					   (foreign-type-size '(:struct _my-ip-address-class))
					   (callback my-ip-address-class-init)
					   (foreign-type-size '(:struct _my-ip-address))
					   (callback my-ip-address-init)
					   0)))
    entry-type))

(defun my-ip-address-new ()
  (g-object-newv "MyIPAddress" 0 (cffi:null-pointer)))

(defcfun ("gtk_entry_set_text" gtk-entry-set-text) :void (entry :pointer) (text :string))

(defun my-ip-address-render (ip-address)
  (let ((priv (g-type-instance-get-private ip-address (my-ip-address-get-type-simple))))
    (let ((str (format nil "~a" (mem-ref
				 (foreign-slot-value priv '(:struct _my-ip-address-private)
						     'address)
				 :int
				 0))))
      (gtk-entry-set-text ip-address str))))



(defcfun ("g_type_check_instance_cast" g-type-check-instance-cast) :pointer (instance :pointer) (iface-type g-type))

(defcfun ("gtk_widget_get_type" gtk-widget-get-type) g-type)

(defun my-ip-address-get-address (ip-address)
  (let ((priv (g-type-instance-get-private ip-address (my-ip-address-get-type-simple))))
    (format nil "~a~%" (loop for i below 4 collect
			    (mem-ref
			     (foreign-slot-value priv
						 '(:struct _my-ip-address-private)
						 'address)
			     :int
			     i)))))

(defcfun ("g_signal_emit_by_name" g-signal-emit-by-name) :void
  (instance :pointer)
  (detailed-signal :string)
  &rest)

(defun my-ip-address-set-address (ip-address ip)
  (let ((priv (g-type-instance-get-private ip-address (my-ip-address-get-type-simple))))
    (format nil "~a~%" (loop for i below 4 collect
			    (setf
			     (mem-ref
			      (foreign-slot-value priv
						  '(:struct _my-ip-address-private)
						  'address)
			      :int
			      i)
			     (min 255 (max 0 (elt ip i)))))))
  (my-ip-address-render ip-address)
  (g-signal-emit-by-name ip-address "ip-changed"))

#+nil
(sb-int:with-float-traps-masked (:divide-by-zero)
  (within-main-loop
    (let ((window (make-instance 'gtk-window :title "test"
				 :default-width 480
				 :default-height 200
				 :border-width 12
				 :type :toplevel
				 )))
      (g-signal-connect window "destroy" (lambda (widget)
					   (leave-gtk-main)))
      (my-ip-address-get-type-simple)
      (let ((ip-address (my-ip-address-new)))
	(my-ip-address-set-address ip-address '(1 2 3 4))
	#+nil (g-signal-connect ip-address "ip-changed"
			  (lambda (ip-address)
			    (format t "ip-changed-inmain: ~a~%"
				    (my-ip-address-get-address ip-address))))
	(gtk-container-add window ip-address))
      (gtk-widget-show-all window))))

