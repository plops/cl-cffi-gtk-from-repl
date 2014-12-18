(defpackage :custom-class
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl)
  (:export #:g-type-instance-get-private
	   #:g-signal-emit-by-name
	   #:%g-signal-connect-data
	   #:gtk-editable-get-type
	   #:gtk-entry-set-text
	   #:g-type-check-instance-cast
	   #:gtk-widget-get-type))

