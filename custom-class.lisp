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
     (otherwise (format t "invalid property id.")))))

;; G_OB..WARN_INVALID_PROPERTY_ID obj property_id pspec is defined as:

;; G_OB..WARN_INVALID_PSPEC obj "property" property-id pspec is defined as:

;; #define G_OBJECT_WARN_INVALID_PSPEC(object, pname, property_id, pspec) \
;; G_STMT_START { \ ; this means: do
;;   GObject *_glib__object = (GObject*) (object); \
;;   GParamSpec *_glib__pspec = (GParamSpec*) (pspec); \
;;   guint _glib__property_id = (property_id); \
;;   g_warning ("%s: invalid %s id %u for \"%s\" of type '%s' in '%s'", \
;;              G_STRLOC, \
;;              (pname), \
;;              _glib__property_id, \
;;              _glib__pspec->name, \
;;              g_type_name (G_PARAM_SPEC_TYPE (_glib__pspec)), \
;;              G_OBJECT_TYPE_NAME (_glib__object)); \
;; } G_STMT_END ; this means while(0)

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
  (let* ((priv (g-type-instance-get-private object (my-ip-address-get-type-simple)))
	 (ad (foreign-slot-value priv '(:struct _my-ip-address-private) 'address)))
    (case prop-id
      (*prop-ip1* (g-value-set-int value (mem-aref ad :int 0)))
      (*prop-ip2* (g-value-set-int value (mem-aref ad :int 1)))
      (*prop-ip3* (g-value-set-int value (mem-aref ad :int 2)))
      (*prop-ip4* (g-value-set-int value (mem-aref ad :int 3)))
      (otherwise (format t "invalid property id.")))))

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


(defcfun ("g_signal_connect_data" %g-signal-connect-data) :ulong
  (instance :pointer)
  (detailed-signal :string)
  (c-handler :pointer)
  (data :pointer)
  (destroy-data :pointer) ;; fun(void*data,GClosure*closure)
  (connect-flags gtk::connect-flags))

(defcallback my-ip-address-init :void ((ip-address :pointer))
  (let* ((priv (g-type-instance-get-private ip-address (my-ip-address-get-type-simple)))
	 (ad (foreign-slot-value priv '(:struct _my-ip-address-private) 'address)))
    (dotimes (i 4)
      (setf (mem-aref ad :int i) 0))
    (let ((fd (pango-font-description-from-string "Monospace"))) ;; fd doesn't need to be freed
      (gtk-widget-modify-font (g-type-check-instance-cast ip-address
							  (gtk-widget-get-type))
			      fd)
      (my-ip-address-render ip-address))
    (%g-signal-connect-data (verify-g-object ip-address) "key-press-event" (callback my-ip-address-key-pressed)
			    (cffi:null-pointer) (cffi:null-pointer) 0)
    (%g-signal-connect-data (verify-g-object ip-address) "notify::cursor-position" (callback my-ip-address-move-cursor)
			    (cffi:null-pointer) (cffi:null-pointer) 0)))
#+nil
(g-type-fundamental (ash 20 2))

(defun verify-g-object (x)
  (g-type-check-instance-cast x (ash 20 2)))

(defcfun ("gtk_editable_get_type" gtk-editable-get-type) g-type)

(defcallback my-ip-address-move-cursor :void
    ((entry :pointer) ;; GObject
     (spec (:pointer g-param-spec)))
  (defparameter *blap1* entry)
  (let* ((ed (g-type-check-instance-cast entry (gtk-editable-get-type)))
	 ;; this still gives the error Gtk-CRITICAL **:
	 ;; gtk_editable_get_position: assertion 'GTK_IS_EDITABLE
	 ;; (editable)' failed
	 (cursor (gtk-editable-get-position ed)))
    (format t "cursor moved to ~d.~%" cursor)
    (gtk-editable-set-position ed
			       (cond ((<= cursor 3) 3)
				     ((<= cursor 7) 7)
				     ((<= cursor 11) 11)
				     (t 15)))))


(defcallback my-ip-address-key-pressed :boolean ((entry :pointer) ; GObject
						 (event :pointer) ; GdkEventKey
						 )
  (format t "key prdessed ~a " (list (foreign-slot-value event '(:struct %gdk-event-key) 'type)
				     (foreign-slot-value event '(:struct %gdk-event-key) 'keyval)
				     (foreign-slot-value event '(:struct %gdk-event-key) 'length)
				     (foreign-slot-value event '(:struct %gdk-event-key) 'string)))
  (defparameter *blap2* entry)
  
  (let* ((k (foreign-slot-value event '(:struct %gdk-event-key) 'keyval))
	 (ed (g-type-check-instance-cast entry (gtk-editable-get-type)))
	 (priv (g-type-instance-get-private entry (my-ip-address-get-type-simple)))
	 (ad (foreign-slot-value priv '(:struct _my-ip-address-private) 'address)))
    (cond ((<= (gdk-unicode-to-keyval #\0) k (gdk-unicode-to-keyval #\9)) ;; fixme keypad doesn't work
	   (let* ((cursor (floor (gtk-editable-get-position ed) 4))
		  (value (read-from-string (foreign-slot-value event '(:struct %gdk-event-key) 'string))))
	     (format t " ~a" (list cursor value))
	     (when (and (= 25 (mem-aref ad :int cursor)) ;; prevent entering bigger than 255
			(< 5 value))
	       (return-from my-ip-address-key-pressed T))
	     (when (< (mem-aref ad :int cursor) 26)
	       (setf (mem-aref ad :int cursor) (+ value (* 10 (mem-aref ad :int cursor))))
	       (progn (my-ip-address-render entry)
		      (gtk-editable-set-position ed (+ (* 4 cursor) 3))
		      (g-signal-emit-by-name entry "ip-changed")))))
	  ((= k (gdk-unicode-to-keyval #\Tab)) ;; move to next number or wrap around to first
	   (let ((cursor (1+ (floor (gtk-editable-get-position ed) 4))))
	     (gtk-editable-set-position ed (+ 3 (* 4 (mod cursor 4))))))
	  ((= k (gdk-unicode-to-keyval #\Backspace)) ;; integer divide by 10 to delete last digit
	   (let ((cursor (floor (gtk-editable-get-position ed) 4)))
	     (setf (mem-aref ad :int cursor) (floor (mem-aref ad :int cursor) 10))
	     (progn (my-ip-address-render entry)
		      (gtk-editable-set-position ed (+ (* 4 cursor) 3))
		      (g-signal-emit-by-name entry "ip-changed"))))
	  ((= k (gdk-unicode-to-keyval #\Return)) ;; activate entry widget
	   (gtk-widget-activate entry))))
  (format t "~%")
  
  T)
#+nil
(defcstruct )
#+nil
(foreign-slot-value *event* '(:struct %gdk-event-key) 'keyval)
#+nil
(foreign-slot-value *event* '(:struct %gdk-event-key) 'string)

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
  (let* ((priv (g-type-instance-get-private ip-address (my-ip-address-get-type-simple)))
	 (ad (foreign-slot-value priv '(:struct _my-ip-address-private) 'address)))
    (let ((str (format nil "~3d.~3d.~3d.~3d"
		       (mem-aref ad :int 0)
		       (mem-aref ad :int 1)
		       (mem-aref ad :int 2)
		       (mem-aref ad :int 3))))
      (gtk-entry-set-text ip-address str))))



(defcfun ("g_type_check_instance_cast" g-type-check-instance-cast) :pointer (instance :pointer) (iface-type g-type))

(defcfun ("gtk_widget_get_type" gtk-widget-get-type) g-type)

(defun my-ip-address-get-address (ip-address)
  (let* ((priv (g-type-instance-get-private ip-address (my-ip-address-get-type-simple)))
	 (ad (foreign-slot-value priv '(:struct _my-ip-address-private) 'address)))
    (format nil "~a~%" (loop for i below 4 collect
			    (mem-aref ad :int i)))))

(defcfun ("g_signal_emit_by_name" g-signal-emit-by-name) :void
  (instance :pointer)
  (detailed-signal :string)
  &rest)

(defun my-ip-address-set-address (ip-address ip)
  (let* ((priv (g-type-instance-get-private ip-address (my-ip-address-get-type-simple)))
	 (ad (foreign-slot-value priv '(:struct _my-ip-address-private) 'address)))
    (format t "new address ~a~%" (loop for i below 4 collect
			  (setf (mem-aref ad :int i) (min 255 (max 0 (elt ip i)))))))
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
	(defparameter *blap* ip-address)
	(my-ip-address-set-address ip-address '(4 3 2 1))
	#+nil (g-signal-connect ip-address "ip-changed"
			  (lambda (ip-address)
			    (format t "ip-changed-inmain: ~a~%"
				    (my-ip-address-get-address ip-address))))
	(gtk-container-add window ip-address))
       (gtk-widget-show-all window))))

#+nil
(g-type-check-instance-type *blap* (gtk-editable-get-type))
#+nil
(g-type-check-instance-cast *blap* (gtk-editable-get-type))
#+nil
(gtk-editable-get-position *blap*)
#+nil
(gtk-editable-get-position *blap2*)
#+nil
(gtk-editable-set-position *blap1* 1)
#+nil
(my-ip-address-get-address *blap*)
#+nil
(my-ip-address-set-address *blap* '(1 2 1 100))
