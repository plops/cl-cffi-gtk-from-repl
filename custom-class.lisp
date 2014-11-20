(in-package :custom-class)

;; http://scentric.net/tutorial/sec-custom-cell-renderers.html


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
    entry-type))

#+nil
(my-ip-address-get-type-simple)


