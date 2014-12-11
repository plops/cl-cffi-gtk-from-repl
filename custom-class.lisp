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
(defcstruct _my-ip-address-class (parent-class (:struct %gtk-entry-class)))
(defparameter *prop-ip1* 1)
(defparameter *prop-ip2* 2)
(defparameter *prop-ip3* 3)
(defparameter *prop-ip4* 4)
(defcallback my-ip-address-set-property :void ((object :pointer)
					       (prop-id :unsigned-int)
					       (value :pointer)
					       (pspec :pointer))
  (let ((address (make-array 4 :initial-element -1 :element-type 'fixnum)))
   (case prop-id
     (*prop-ip1* (setf (aref address 0) (g-value-get-int value))
		 (my-ip-set-address object address))
     ;; fixme more cases necessary
     )))

(defcallback my-ip-address-class-init :void ((klass :pointer) (data (g-object gpointer)))
  (declare (ignore data))
  ;; REGISTER signals
  (defparameter *class-init* klass)
  (format t "~A~%" (list 'class-init #+nil (foreign-slot-value klass '%gobject-class) 'set-property))
  (setf (foreign-slot-value klass '(:struct %gobject-class) 'set-property) (callback my-ip-address-set-property))
  )
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
					   "MyIPAddress"
					   (foreign-type-size '(:struct _my-ip-address-class))
					   (callback my-ip-address-class-init)
					   (foreign-type-size '(:struct _my-ip-address))
					   (callback my-ip-address-init)
					   0)))
    entry-type))

#+nil
(my-ip-address-get-type-simple)

(defun my-ip-address-new ()
  (g-object-new "MyIPAddress"))

#+nil
(g-object-newv "MyIPAddress" 0 (cffi:null-pointer))

#+nil
(g-object-newv "GTKEntry" 0 (cffi:null-pointer))

#+nil
(defparameter *bla* (my-ip-address-new))
