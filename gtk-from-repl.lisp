;; How to develop a GTK graphical user interface with Common Lisp and
;; ease

;; I deal mainly with computer algorithms in image processing and need
;; therefore need a simple way to display mages and animations. My
;; previous attempts with the open libraries LTK (Lisp binding for the
;; TK library), mcclim or common qt have unfortunately failed.

;; Some time ago I discovered cl-cffi-gtk. This is a foreign function
;; binding to call GTK+ 3 from Common Lisp. After some experimenting I
;; arrived at a point where I can create graphical interfaces rather
;; efficiently. For this, I think it is important that I can
;; incrementally replace the GUI widgets at runtime, without having to
;; restart the Lisp image.

;; First of all the package cl-cffi-gtk must be loaded. Then I define
;; the package myg for my own code. I structured my source code so
;; that the file can be easily compiled using the keyboard shortcut
;; C-c C-k in a SLIME session in Emacs.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))

(defpackage :myg
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :myg)

;; While GTK+ is a C library its interface is object-oriented. In
;; cl-cffi-gtk the widgets are CLOS classes classes and their
;; parameters can be defined either during instantiation or later
;; using various methods. The following function run-0 contains
;; minimal code to create a window without any widgets.

(defun run-0 ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window :title "myg-window"
				   :default-width 580
				   :default-height 200
				   :border-width 12
				   :type :toplevel)))
	(g-signal-connect window "destroy"
			  (lambda (widget)
			    (declare (ignorable widget))
			    (leave-gtk-main)))
	(gtk-widget-show-all window)))))

#+nil
(run-0)

;; When the user closes the application window using the window
;; manager, the instance window emits the "destroy" signal. The lambda
;; function in the previous code will leave the main applications main
;; loop and shut down the program (but not the lisp image).

;; The following function run-1 shows how to add a button to the window.

(defun run-1 ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window :title "myg-window"
				   :default-width 580
				   :default-height 200
				   :border-width 12
				   :type :toplevel)))
	(g-signal-connect window "destroy"
			  (lambda (widget)
			    (declare (ignorable widget))
			    (leave-gtk-main)))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(let ((button (make-instance 'gtk-button :label "test")))
	  (gtk-container-add window button))
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(gtk-widget-show-all window)))))

#+nil
(run-1)

;; Widgets can receive different signals. Usually I get a good
;; overview of possible signals using Glade (see bottom right corner
;; in the screenshot).

(progn
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *button* nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun run-2 ()
    (sb-int:with-float-traps-masked (:divide-by-zero)
      (within-main-loop
	(let ((window (make-instance 'gtk-window :title "myg-window"
				     :default-width 580
				     :default-height 200
				     :border-width 12
				     :type :toplevel)))
	  (g-signal-connect window "destroy"
			    (lambda (widget)
			      (declare (ignorable widget))
			      (leave-gtk-main)))
	  (let ((button (make-instance 'gtk-button :label "test")))
	    (gtk-container-add window button)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    (setf *button* button)
	    (g-signal-connect button "clicked"
			      (lambda (widget)
				(declare (ignorable widget))
				(format t "button has been clicked~%")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	    )
	  (gtk-widget-show-all window))))))

#+nil
(run-2)

;; In run 2, I save the object button in a global variable
;; *button*. The slime Inspector (C-c C-I) shows the following when
;; applied to this object.


;; #<GTK-BUTTON {100C50D883}>
;; --------------------
;; Class: #<GOBJECT-CLASS GTK-BUTTON>
;; --------------------
;;  Group slots by inheritance [ ]
;;  Sort slots alphabetically  [X]

;; All Slots:
;; [ ]  ACTION-NAME           = NIL
;; [ ]  ACTION-TARGET         = #.(SB-SYS:INT-SAP #X00000000)
;; [ ]  ALWAYS-SHOW-IMAGE     = NIL
;; [ ]  APP-PAINTABLE         = NIL
;; [ ]  BORDER-WIDTH          = 0
;; [ ]  CAN-DEFAULT           = NIL
;; [ ]  CAN-FOCUS             = T
;; [ ]  CHILD                 = #<unbound>
;; [ ]  COMPOSITE-CHILD       = NIL
;; [ ]  DOUBLE-BUFFERED       = T
;; [ ]  EVENTS                = NIL
;; [ ]  EXPAND                = NIL
;; [ ]  FOCUS-ON-CLICK        = T
;; [ ]  HALIGN                = :FILL
;; [ ]  HAS-DEFAULT           = NIL
;; [ ]  HAS-FOCUS             = NIL
;; [ ]  HAS-REFERENCE         = T
;; [ ]  HAS-TOOLTIP           = NIL
;; [ ]  HEIGHT-REQUEST        = -1
;; [ ]  HEXPAND               = NIL
;; [ ]  HEXPAND-SET           = NIL
;; [ ]  IMAGE                 = NIL
;; [ ]  IMAGE-POSITION        = :LEFT
;; [ ]  IS-FOCUS              = T
;; [ ]  LABEL                 = "test"
;; [ ]  MARGIN                = 0
;; [ ]  MARGIN-BOTTOM         = 0
;; [ ]  MARGIN-LEFT           = 0
;; [ ]  MARGIN-RIGHT          = 0
;; [ ]  MARGIN-TOP            = 0
;; [ ]  NAME                  = ""
;; [ ]  NO-SHOW-ALL           = NIL
;; [ ]  OPACITY               = 1.0d0
;; [ ]  PARENT                = #<GTK-WINDOW {100DE8AEE3}>
;; [ ]  POINTER               = #.(SB-SYS:INT-SAP #X7FFFE007A350)
;; [ ]  RECEIVES-DEFAULT      = T
;; [ ]  RELATED-ACTION        = NIL
;; [ ]  RELIEF                = :NORMAL
;; [ ]  RESIZE-MODE           = :PARENT
;; [ ]  SENSITIVE             = T
;; [ ]  SIGNAL-HANDLERS       = #()
;; [ ]  STYLE                 = #<GTK-STYLE {100DE8AF23}>
;; [ ]  TOOLTIP-MARKUP        = NIL
;; [ ]  TOOLTIP-TEXT          = NIL
;; [ ]  USE-ACTION-APPEARANCE = T
;; [ ]  USE-STOCK             = NIL
;; [ ]  USE-UNDERLINE         = NIL
;; [ ]  VALIGN                = :FILL
;; [ ]  VEXPAND               = NIL
;; [ ]  VEXPAND-SET           = NIL
;; [ ]  VISIBLE               = T
;; [ ]  WIDTH-REQUEST         = -1
;; [ ]  WINDOW                = #<GDK-WINDOW {100DE8AF43}>
;; [ ]  XALIGN                = 0.5
;; [ ]  YALIGN                = 0.5

;; [set value]  [make unbound]

;; In addition, I have copied the event handlers of the window and
;; attached it to the signal 'clicked'. In the *inferior-lisp* buffer
;; of Emacs each click of the button produces a line with the text
;; text "button has been clicked".

;; Given the occasion, I would like to point out an error that was
;; printed in the *inferior-lisp* buffer instead of the desired text
;; output when my definition of the signal handler looked like that:

;; (g-signal-connect window "clicked"
;; 		      (lambda (widget)
;; 			(declare (ignorable widget))
;; 			(format t "button has been clicked~%")))


;; (sbcl:7507): GLib-GObject-WARNING **:
;; /var/tmp/portage/dev-libs/glib-2.40.0-r1/work/glib-2.40.0/gobject/gsignal.c:2362:
;; signal 'clicked' is invalid for instance '0x7fffe0005110' of type
;; 'GtkWindow'

;; In this case, I accidentally wrote "window" instead of "button"
;; "window" and the window object supports no signal 'clicked'.

;; Going back to the output of the inspector of the object button, I
;; want to emphasize this line:
;; [ ] LABEL = "test"

;; I would like to convert the program to a dice generator, that
;; displays a random number between 1 and 6 in the button label. To
;; find out how I can incorporate this modification I press M-. on the
;; class definition #<GOBJECT CLASS GTK-BUTTON> in the third row of
;; the Inspector (alternatively one can place the cursor on gtk-button
;; in the call to make-instance). As a result I jump the definition of
;; the gtk-button class in cl-cffi-gtk's source code at
;; ~/quicklisp/dists/quicklisp/software/cl-cffi-GTK-20141006-git/GTK/GTK.button.Lisp
;; This place looks like this:

;; (define-g-object-class "GtkButton" gtk-button
;;   (:superclass gtk-bin
;;    :export t
;;    :interfaces ("AtkImplementorIface"
;;                 "GtkBuildable"
;;                 "GtkActionable"
;;                 "GtkActivatable")
;;    :type-initializer "gtk_button_get_type")
;;   ....
;;    (image-position
;;     gtk-button-image-position
;;     "image-position" "GtkPositionType" t t)
;;    (label
;;     gtk-button-label
;;     "label" "gchararray" t t)
;;    (relief
;;     gtk-button-relief
;;     "relief" "GtkReliefStyle" t t)
;;     ....

;; After some experimenting, I realized that gtk-button-label is the
;; name of the method to read and change the button's text label:
    
;; (gtk-button-label *button*) => "test"
;; (setf (gtk-button-label *button*) "1")  => aendert Button Label zu "1"

(progn
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defparameter *button* nil)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun run-3 ()
    (sb-int:with-float-traps-masked (:divide-by-zero)
      (within-main-loop
	(let ((window (make-instance 'gtk-window :title "dice"
				     :default-width 128
				     :default-height 20
				     :border-width 12
				     :type :toplevel)))
	  (g-signal-connect window "destroy"
			    (lambda (widget)
			      (declare (ignorable widget))
			      (leave-gtk-main)))
	  (let ((button (make-instance 'gtk-button :label "click for roll")))
	    (gtk-container-add window button)

	    (setf *button* button)
	    (g-signal-connect button "clicked"
			      (lambda (widget)
				(declare (ignorable widget))
					    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				(setf (gtk-button-label *button*) (format nil "~a"
									  (+ 1 (random 5))))
					    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				)))
	  (gtk-widget-show-all window))))))

#+nil
(run-3)

;; The function run-3 is the first reasonable GTK application of this
;; post.


;; Now, I want to build a more interesting application. As the
;; screenshot below shows it consists of a cairo canvas on its left
;; side. In it, I paint a circle whose of center coordinates and
;; radius shall be controlled by GUI widgets on the right side of the
;; window.

;; A gtk-paned widget splits the window using a divider that can be
;;  adjusted by the user. I store this gtk-paned widget in a global
;;  variable *paned*, so that later I can access it and all its child
;;  widgets. Additionally I also decided to store the cairo canvas in
;;  the global variable *canvas* so that I can force it to redraw
;;  whenever GUI input widgets change their parameters.

(defparameter *paned* nil)
(defparameter *canvas* nil)

;; Using the Cairo canvas is a chapter in itself and is introduced
;; very well in the cl-cffi-gtk tutorial on:
;; http://www.crategus.com/books/CL-GTK/GTK-tutorial_16.html#SEC172

;; Essentially, you define a draw function that calls cairo functions
;; to set up a state machine and draw lines and curves with specific
;; colors and coordinate transforms. The semantics are very similar to
;; those of PostScript.

;; The following simple function paints a purple circle and one
;; red line on the canvas:

(progn
  (defun draw-canvas (widget cr)
    (declare (ignorable widget))
    (let ((cr (pointer cr)))
      (cairo-set-source-rgb cr 1.0 1.0 1.0)
      (cairo-scale cr 1 1)
      (cairo-paint cr)     
      (let* ((radius (or (spin-button-value 'radius *paned*) 100d0))
	     (angle (* (/ pi 180) (or (spin-button-value 'angle *paned*) 1d0)))
	     (x (or (spin-button-value 'xpos *paned*) 100d0))
	     (y (or (spin-button-value 'ypos *paned*) 80d0)))
	(cairo-arc cr x y radius 0 (* 2 pi))
	(cairo-set-source-rgb cr 1 0 1) ;; r g b => violet circle
	(cairo-stroke cr)
	(cairo-save cr)
	(cairo-set-source-rgb cr 1 0 0)
	(cairo-move-to cr x y)
	(cairo-line-to cr
		       (+ x (* radius (sin angle)))
		       (+ y (* radius (- (cos angle)))))
	(cairo-stroke cr)  ;; draw a red line
	(cairo-restore cr))
      (cairo-destroy cr)
      t))
  (defparameter *draw-canvas* #'draw-canvas))

;; First, I would like to say something about the peculiar function
;; definition. I store the function draw-canvas in the global variable
;; *draw-canvas*. Later in the 'draw' signal handlers for the canvas I
;; will call the function from this global variable using
;; funcall. This allows me to redefine the function during run-time
;; and if there are errors while compiling draw-canvas, the call to
;; (defparameter *draw-canvas* #' draw-canvas) will not be executed,
;; keeping the old working function intact. Only with successful
;; compilation the value in *draw-canvas* is replaced and the new draw
;; function will be used. (There may still be other errors that the
;; compilation doesn't step but in my experience this method catches
;; quite a few unnecessary bugs which would otherwise force to restart
;; the lisp image).

;; Eventually, the variables radius, angle the coordinates x and y
;; shall be obtained from the GUI input widgets. But as I haven't
;; described them let's just assume the following stub definition
;; returning always nil:

#+nil
(defun spin-button-value (name paned)
  "Return the adjustment value of the spin-button that is labeled with NAME."
  nil)

;; When spin-button-value returns nil, the calls to "or" in
;; draw-canvas will evaluate to the second parameter with a numerical
;; constant.

;; The following function run-4 constructs a the GUI window as before.
;; First it creates a top-level window. Then attached as gtk-paned
;; object create a vertical division.  The left section is filled with
;; a cairo canvas whose dimensions are set to 1024x1024. If the
;; top-level window is too small, the scrolled-window objects adds
;; scrollbars. The right side of gtk-paned is filed with a vertical
;; box of gtk-spinboxes.

(defun run-4 ()
  (sb-int:with-float-traps-masked (:divide-by-zero)
    (within-main-loop
      (let ((window (make-instance 'gtk-window :title "myg-window"
				   :default-width 580
				   :default-height 200
				   :border-width 12
				   :type :toplevel)))
	(g-signal-connect window "destroy"
			  (lambda (widget)
			    (declare (ignorable widget))
			    (leave-gtk-main)))
	(let ((paned (make-instance 'gtk-paned :orientation :horizontal :position 400)))
	  (let ((scrolled (make-instance 'gtk-scrolled-window
					 :border-width 1
					 :hscrollbar-policy :automatic
					 :vscrollbar-policy :automatic))
		(canvas (make-instance 'gtk-drawing-area)))
	    (setf *paned* paned
		  *canvas* canvas)
	    (g-signal-connect canvas "draw"
			      (lambda (widget cr)
				(funcall *draw-canvas* widget cr)))
	    (gtk-scrolled-window-add-with-viewport scrolled canvas)
	    (setf (gtk-widget-size-request canvas) (list 1024 1024))
	    (gtk-container-add window paned)
	    (gtk-paned-add1 paned scrolled)
	    
	    (let* ((vbox (make-instance 'gtk-box :orientation :vertical)))
	      (add-spinbox-to-vbox vbox 'xpos 70 1024 canvas)
	      (add-spinbox-to-vbox vbox 'ypos 80 1024 canvas)
	      (add-spinbox-to-vbox vbox 'radius 50 500 canvas)
	      (add-spinbox-to-vbox vbox 'angle 0 360 canvas)
	      (gtk-paned-add2 paned vbox))))
	(gtk-widget-show-all window)))))

;; I believe that a spin box is the best input widget to represent my
;; input requirements. A spin box consists a number and next to it are
;; up and down arrows that allow to adjust the value by mouse
;; clicks. Alternatively, the value can be directly entered via the
;; text input field or changed by key presses (arrows or
;; page-up/down). The value range and that the step size are
;; represented by the gtk-adjustment class. When the value changes, a
;; request for a redraw is sent to the canvas using
;; gtk-widget-queue-draw.

(defun add-spinbox-to-vbox (container name value upper canvas)
  "Make a horizontal box containing a label on the left and a spin
button right of it and add it to container. Changing a value will
signal canvas."
  (let* ((hb (make-instance 'gtk-box :orientation :horizontal))
	 (lab (make-instance 'gtk-label
			     :label (symbol-name name)))
	 (adj (make-instance 'gtk-adjustment
			     :value (* 1d0 value)
			     :lower 0d0
			     :upper (* 1d0 upper)
			     :step-increment 1d0
			     :page-increment 10d0
			     :page-size 0d0))
	 (sb (make-instance 'gtk-spin-button :adjustment adj
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
    (gtk-box-pack-start container hb)
    hb))

;; If the canvas is redrawn by the function draw-canvas, then one of
;; the calls is (spin-button-value 'xpos *paned*), which reads the
;; current numerical value from the widget with the label "XPOS". In
;; order to do this, the spin-button-value function starts from the
;; object *paned* and traverses into the (vertical) box on the right
;; side, i.e. the second element of: (gtk-container-get-children
;; *paned*):

;; => (#<GTK-SCROLLED-WINDOW {100A865843}> #<GTK-BOX {100A9F7583}>)


;; Then it searches for the requested symbol name in the label text of
;; each spin box and returns the numerical value that is stored in the
;; adjustment.

(defun spin-button-value (name paned)
  "Return the adjustment value of the spin-button that is labeled with NAME."
  (let ((hbox-children (find-if #'(lambda (x)
				    (string= (symbol-name name) (gtk-label-get-text (first x))))
				(mapcar #'gtk-container-get-children (gtk-container-get-children
				  (second (gtk-container-get-children paned)))))))
    (when hbox-children
      (gtk-adjustment-get-value (gtk-spin-button-get-adjustment (second hbox-children))))))

;; Ein Beispielaufruf ist der Folgende:

#+nil
(spin-button-value 'ypos *paned*) ;; => 75.0


;; Durch Aufruf von run-4 wird das Fenster mit dem Cairo Canvas
;; geoeffnet, das im Screenshot dargestellt ist.

#+nil
(run-4)

;; Der folgende Aufruf hangelt sich von *paned* zur vertikalen Box und
;; l\"oscht alle darin enthaltenen Widgets. Uebrig bleibt nur der
;; Cairo Canvas.

#+nil
(gtk-widget-destroy (second (gtk-container-get-children *paned*)))

;; Durch den folgenden Aufruf werden in der laufenden GTK Applikation
;; wieder Spinboxen erzeugt. Die jedoch mit anderen Defaultwerten
;; initialisiert sind:

#+nil
(let* ((vbox (make-instance 'gtk-box :orientation :vertical)))
  (add-spinbox-to-vbox vbox 'xpos 100 1024 *canvas*)
  (add-spinbox-to-vbox vbox 'ypos 154 1024 *canvas*)
  (add-spinbox-to-vbox vbox 'radius 50 500 *canvas*)
  (add-spinbox-to-vbox vbox 'angle 30 360 *canvas*)
  (gtk-paned-add2 *paned* vbox)
  (gtk-widget-show-all *paned*))

;; Auf diese Weise kann man zum Beispiel auch zusaetzliche Spinboxen
;; mit anderen Labeltexten als XPOS, YPOS, RADIUS oder ANGLE erzeugen
;; und damit neue Objekte in draw-canvas zeichnen.


