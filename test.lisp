(setf asdf:*central-registry*
      ;; Default directories, usually just the ``current directory''
  '(*default-pathname-defaults*
    ;; Additional places where ASDF can find
    ;; system definition files
    #+linux #p"/home/martin/cl-cffi-gtk-from-repl/"))
#+nil
(time 
 (asdf:load-system "custom-widget"))

;; on my x201 (520 moderate fan speed)
  ;; 52.570 seconds of real time
  ;; 48.156000 seconds of total run time (46.996000 user, 1.160000 system)
  ;; [ Run times consist of 2.292 seconds GC time, and 45.864 seconds non-GC time. ]
  ;; 91.60% CPU
  ;; 52,876 forms interpreted
  ;; 128,756 lambdas converted
  ;; 125,851,719,666 processor cycles
  ;; 19 page faults
;; 6,061,797,824 bytes consed1
;; full fan speed second test
 ;; Evaluation took:
 ;;  57.228 seconds of real time
 ;;  53.216000 seconds of total run time (51.832000 user, 1.384000 system)
 ;;  [ Run times consist of 2.548 seconds GC time, and 50.668 seconds non-GC time. ]
 ;;  92.99% CPU
 ;;  52,876 forms interpreted
 ;;  128,756 lambdas converted
 ;;  137,003,156,417 processor cycles
 ;;  19 page faults
 ;;  6,065,057,760 bytes consed

;; on acer-gpu Intel(R) Core(TM) i5-2430M CPU @ 2.40GHz
;; Evaluation took:
;;   46.925 seconds of real time
;;   38.532000 seconds of total run time (37.648000 user, 0.884000 system)
;;   [ Run times consist of 1.680 seconds GC time, and 36.852 seconds non-GC time. ]
;;   82.11% CPU
;;   52,901 forms interpreted
;;   128,727 lambdas converted
;;   112,365,232,738 processor cycles
;;   18 page faults
;;   6,057,826,464 bytes consed
  

;; on new x201 (540 full fan speed)
;; Evaluation took:
;;   50.881 seconds of real time
;;   46.860000 seconds of total run time (45.932000 user, 0.928000 system)
;;   [ Run times consist of 2.148 seconds GC time, and 44.712 seconds non-GC time. ]
;;   92.10% CPU
;;   52,876 forms interpreted
;;   128,756 lambdas converted
;;   128,574,849,282 processor cycles
;;   19 page faults
;;   6,061,191,376 bytes consed
  

(in-package :custom-class)


(cffi:foreign-type-size '(:struct %gtk-cell-renderer))
custom-class:size-of-%gtk-cell-renderer

