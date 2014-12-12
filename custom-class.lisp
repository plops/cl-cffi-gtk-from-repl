(in-package :custom-class)

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
(defcstruct _my-ip-address-private (address :uint :count 4))

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
		 (my-ip-set-address object address))
     ;; fixme more cases necessary
     )))

(defparameter *changed-signal* 0)
(defparameter *my-ip-address-signal* (make-array 1 :element-type '(unsigned-byte 64)))

(defcfun ("g_type_instance_get_private" g-type-instance-get-private)
    :pointer
  (instance :pointer) ;; GTypeInstance
  (private-type g-type) ;; GType is a numerical value
  )

#+nil
(defparameter *bla2* (g-type-instance-get-private *class-init* (gtype-id (my-ip-address-get-type-simple))))
;; gtype-id is optional
#+nil
(defparameter *bla3* (g-type-instance-get-private *class-init* (my-ip-address-get-type-simple)))

(defcallback my-ip-address-get-property :void ((object :pointer)
					       (prop-id :unsigned-int)
					       (value :pointer)
					       (parameter-spec :pointer))
  ;; fixme get private ip address from object
  ;(g-type-instance-get-private object )
  (case prop-id
    (*prop-ip1* (g-value-set-int value 0))
    ;; fixme the other cases
    ))

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
  ;; REGISTER signals
  (defparameter *class-init* klass)
  (format t "~A~%" (list 'class-init #+nil (foreign-slot-value klass '%gobject-class) 'set-property))
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
  (g-object-class-install-property klass
				   *prop-ip1*
				   (g-param-spec-int "ip-number-1"
						     "IP Address Number 1"
						     "The first IP address number"
						     0 255 0 '(:readable :writable)))
  (format t "signal ip-changed has been created~%"))

#+nil
(foreign-slot-offset '(:struct _my-ip-address-class)
		     'ip-changed) ; => 976 as in C

#+nil
(foreign-slot-value *class-init* '(:struct %gobject-class) 'set-property)

(defcallback my-ip-address-init :void ((ip-address :pointer))
  (declare (ignore ip-address))
  ;; REGISTER signals
  (format t "~A~%" 'init)
  )

(let ((entry-type nil))
  (defun my-ip-address-get-type-simple ()
    (unless entry-type
      (setf entry-type
	    (g-type-register-static-simple (g-type-from-name "GtkEntry")
					   "MyIPAddress2"
					   (foreign-type-size '(:struct _my-ip-address-class))
					   (callback my-ip-address-class-init)
					   (foreign-type-size '(:struct _my-ip-address))
					   (callback my-ip-address-init)
					   0)))
    entry-type))

#+nil
(my-ip-address-get-type-simple)

(defun my-ip-address-new ()
  (g-object-newv "MyIPAddress" 0 (cffi:null-pointer)))

#+nil
(defparameter *bla*
 (g-object-newv "MyIPAddress2" 0 (cffi:null-pointer)))

#+nil
(g-object-newv "GTKEntry" 0 (cffi:null-pointer))

#+nil
(defparameter *bla* (my-ip-address-new))
