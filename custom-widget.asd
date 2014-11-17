(cl:eval-when (:load-toplevel :execute)
  (asdf:operate `asdf:load-op `cffi-grovel))
(asdf:defsystem custom-widget
  :depends-on (cffi cl-cffi-gtk)
  :serial t
  :components
  ((:file "package")
   (cffi-grovel:grovel-file "gtk-grovel")
   (:file "custom-class")))
