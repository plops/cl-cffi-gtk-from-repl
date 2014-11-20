(setf asdf:*central-registry*
      ;; Default directories, usually just the ``current directory''
  '(*default-pathname-defaults*
    ;; Additional places where ASDF can find
    ;; system definition files
    #+linux #p"/home/martin/cl-cffi-gtk-from-repl/"))
#+nil
(asdf:load-system "custom-widget")


(in-package :custom-class)


(cffi:foreign-type-size '(:struct custom-class-internal::%gtk-cell-renderer))
custom-class-internal::size-of-%gtk-cell-renderer

