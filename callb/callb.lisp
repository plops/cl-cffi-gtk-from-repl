(eval-when (:compile-toplevel) (ql:quickload :cffi))

(cffi:load-foreign-library "libcallb.so")

(cffi:defcstruct struct_t
  (set :pointer)
  (callb :pointer)
  (val :int))

(cffi:defcfun "regist" :int  (class-id :int) (callb :pointer))
(cffi:defcfun "unregist" :int  (class-id :int))
(cffi:defcfun "run_callb" :int  (o :pointer) (data :pointer))
(cffi:defcfun "run_set" :int  (o :pointer) (val :int))
(cffi:defcfun "get_type" :pointer (class-id :int))

(cffi:defcallback my-callb :void ((o (:pointer (:struct struct_t))) (data :pointer))
  (declare (ignore data))
  (format t "~A~%" 'my-callb)
  1)

#+nil
(regist 2 (cffi:callback my-callb))
#+nil
(get-type 0)
#+NIL
(run-callb (get-type 2) (cffi:null-pointer))
