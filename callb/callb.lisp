(ql:quickload :cffi)

(cffi:load-foreign-library "libcallb.so")

(cffi:defcstruct struct_t
  (set :pointer)
  (callb :pointer)
  (val :int))

(cffi:defcfun "regist" :int  (class-id :int) (callb :pointer))
(cffi:defcfun "unregist" :int  (class-id :int))
