(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))

(defpackage :myg
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :myg)

(defparameter *adjustments* nil)

(defun spin-button-value (widget-name)
  (let ((hbox-children (gtk-container-get-children (cdr (assoc widget-name *adjustments*)))))
    (when hbox-children
     (gtk-adjustment-get-value (gtk-spin-button-get-adjustment (second hbox-children))))))

(progn
  (defun draw-canvas (widget cr)
    (declare (ignorable widget))
    (let ((cr (pointer cr)))
      (cairo-set-source-rgb cr 1.0 1.0 1.0)
      (cairo-scale cr 1 1)
      (cairo-paint cr)     
      (when *adjustments*
	(let* ((radius (or (spin-button-value 'radius) 100d0))
	       (angle (* (/ pi 180) (or (spin-button-value 'angle) 1d0)))
	       (x (or (spin-button-value 'xpos) 100d0))
	       (y (or (spin-button-value 'ypos) 80d0)))
	  (cairo-arc cr x y radius 0 (* 2 pi))
					;(cairo-set-source-rgb cr 1 1 1)
					;(cairo-fill-preserve cr)
	  (cairo-set-source-rgb cr 1 0 1)
	  (cairo-stroke cr)
	  (cairo-save cr)
	  (cairo-set-source-rgb cr 1 0 0)
	  (cairo-move-to cr x y)
	  (cairo-line-to cr
			 (+ x (* radius (sin angle)))
			 (+ y (* radius (- (cos angle)))))
	  (cairo-stroke cr)
	  (cairo-restore cr)))
      
      (cairo-destroy cr)
      t))

  (defparameter *draw-canvas* #'draw-canvas))



(defparameter *canvas* nil)

(defun spin-box (name value upper canvas)
  "Make a horizontal box containing a label on the left and a spin
button right of it. Changing a value will signal canvas"
  (let* ((hb (make-instance 'gtk-box :orientation :horizontal))
	 (lab (make-instance 'gtk-label
			     :label (format nil "~s" name)))
	 (adj (make-instance 'gtk-adjustment
			     :value (* 1d0 value)
			     :lower 0d0
			     :upper (* 1d0 upper)
			     :step-increment 1d0
			     :page-increment 10d0
			     :page-size 0d0))
	 (sb (make-instance 'gtk-spin-button :adjustment
			    adj
			    :climb-rate 0
			    :digits 1
			    :wrap t)))
    (gtk-spin-button-set-value sb value)
    (gtk-box-pack-start hb lab)
    (gtk-box-pack-start hb sb)
    (g-signal-connect sb "value-changed"
		      (lambda (adjustment)
			(declare (ignorable adjustment))
			(gtk-widget-queue-draw canvas)))
    (push (cons name hb) *adjustments*)
    hb))

(defun run ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window :title "myg-window"
				   :default-width 640
				   :default-height 480
				   :border-width 12
				   :type :toplevel)))
	(g-signal-connect window "destroy"
			  (lambda (widget)
			    (declare (ignorable widget))
			    (leave-gtk-main)))
	(let ((paned (make-instance 'gtk-paned :orientation :horizontal :position 100)))
	  (let ((scrolled (make-instance 'gtk-scrolled-window
					 :border-width 1
					 :hscrollbar-policy :automatic
					 :vscrollbar-policy :automatic))
		(canvas (make-instance 'gtk-drawing-area)))
	    (g-signal-connect canvas "draw"
			      (lambda (widget cr)
				(funcall *draw-canvas* widget cr)))
	    (gtk-scrolled-window-add-with-viewport scrolled canvas)
	    (setf (gtk-widget-size-request canvas) (list 1024 1024))
	    (gtk-container-add window paned)
	    (gtk-paned-add1 paned scrolled)
	    (let* ((vbox (make-instance 'gtk-box :orientation :vertical))
		   (xpos (spin-box 'xpos 1157.5 (- 1920 1) canvas))
		   (ypos (spin-box 'ypos 251.5 (- 1080 1) canvas))
		   (radius (spin-box 'radius 103.5 500 canvas))
		   (angle (spin-box 'angle 0 360 canvas)))
	      (loop for (name . widget) in *adjustments* do
		   (gtk-box-pack-start vbox widget))
	      (defparameter *paned* paned)
	      (gtk-paned-add2 paned vbox))))
	(gtk-widget-show-all window)))))



#+nil
(run)

#+nil
(gtk-widget-destroy *vbox*)
#+nil
(gtk-widget-destroy (first (gtk-container-get-children *frame1*)))

#+nil
(let ((vbox (make-instance 'gtk-box :orientation :vertical)))
  (defparameter *vbox* vbox)
  (loop for (name widget) in `((rb-ft ,(gtk-check-button-new-with-label "ft"))
			       (rb-fit ,(gtk-check-button-new-with-label "fit"))
			       (rb-bla ,(gtk-check-button-new-with-label "bla"))
			       (rb-bla2 ,(gtk-check-button-new-with-label "bla2"))) do
       (gtk-box-pack-start vbox widget))
  (gtk-container-add *frame1* vbox))




#+nil
(list *vbox*
      (gtk-container-get-children *frame1*))

#+nil
(gtk-container-get-children *vbox*)


#+nil
(gtk-widget-show-all *frame1*)
#+nil
(gtk-widget-show-all *vbox*)
