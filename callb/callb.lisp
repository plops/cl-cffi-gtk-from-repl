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

(cffi:defcallback my-callb :int ((o (:pointer (:struct struct_t))) (data :pointer))
  (declare (ignore data))
  (format t "~A~%" 'my-callb)
  (cffi:foreign-slot-value o  '(:struct struct_t) 'val))

#+nil
(regist 0 (cffi:callback my-callb))
#+nil
(get-type 0)
#+nil
(run-callb (get-type 0) (cffi:null-pointer))
#+nil
(run-set (get-type 0) 3)
