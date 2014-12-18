(setf asdf:*central-registry*
      ;; Default directories, usually just the ``current directory''
      #+linux '(#p"/home/martin/cl-cffi-gtk-from-repl/"))
#+nil
(time (asdf:load-system "custom-widget"))
  

(in-package :custom-class)

