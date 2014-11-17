(defpackage :custom-class-internal
  (:use))
(defpackage :custom-class
  (:use :custom-class-internal :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

