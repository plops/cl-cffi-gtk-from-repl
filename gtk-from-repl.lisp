;; How to develop a GTK graphical user interface with Common Lisp and
;; ease Ich beschaeftige mich hauptsaechlich mit Computeralgorithmen
;; zur Bildverarbeitung und brauche daher eine einfache Moeglichkeit
;; Bilder und Animationen anzuzeigen. Meine bisherigen Versuche mit
;; den offenen Bibliotheken LTK (Lisp binding fuer die Tk library),
;; mcclim oder common-qt einzusetzen sind leider gescheitert.

;; Vor einiger Zeit entdeckte ich cl-cffi-gtk. Das ist ein Binding um
;; GTK+ 3 von Common Lisp aus aufzurufen. Nach einigen Experimentieren
;; bin ich jetzt an einem Punkt angelangt wo ich damit effizient
;; graphische Oberflaechen erstellen kann. Dabei finde ich wichtig,
;; dass ich das GUI Widgets inkrementell zur Laufzeit ersetzen kann,
;; ohne das Lisp Image neu starten zu muessen.

;; Zunaechst einmal muss dass Packet cl-cffi-gtk geladen werden und
;; ich definiere ein Packet myg, in dem ich meinen Code schreibe.  Ich
;; strukturiere meinen Quellcode so, dass die Datei in einer Emacs
;; session mit laufendem Slime mit der Tastenkombination C-c C-k
;; compiliert werden kann.


(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-cffi-gtk))

(defpackage :myg
  (:use :gtk :gdk :gobject :glib :pango :cairo :cffi :iterate :cl))

(in-package :myg)

;; Wie dem Screenshot zu entnehmen ist, soll das Fenster auf der
;; linken Seite einen Cairo Canvas zeigen. Darin male ich einen Kreis
;; dessen Mittelpunktskoordinaten und Radius durch GUI Elemente auf
;; der rechten Seite des Fensters eingestellt werden koennen.

;; GTK+ ist eine C library die seine Inhalte auf eine
;; objektorientierte vorhaelt. Fuer die G

(defparameter *paned* nil)
(defparameter *canvas* nil)

(defun spin-button-value (name paned)
  "Return the adjustment value of the spin-button that is labeled with NAME."
  (let ((hbox-children (find-if #'(lambda (x)
				    (string= (symbol-name name) (gtk-label-get-text (first x))))
				(mapcar #'gtk-container-get-children
					(gtk-container-get-children
					 (second (gtk-container-get-children paned)))))))
    (when hbox-children
     (gtk-adjustment-get-value (gtk-spin-button-get-adjustment (second hbox-children))))))

#+nil
(spin-button-value 'ypos)

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
	(cairo-restore cr))
      (cairo-destroy cr)
      t))
  (defparameter *draw-canvas* #'draw-canvas))


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

(defun run ()
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

#+nil
(run)


#+nil
(gtk-container-get-children *paned*) ;; => (#<GTK-SCROLLED-WINDOW {100A865843}> #<GTK-BOX {100A9F7583}>)

#+nil
(gtk-widget-destroy (second (gtk-container-get-children *paned*)))

#+nil
(let* ((vbox (make-instance 'gtk-box :orientation :vertical)))
  (add-spinbox-to-vbox vbox 'xpos 100 1024 *canvas*)
  (add-spinbox-to-vbox vbox 'ypos 154 1024 *canvas*)
  (add-spinbox-to-vbox vbox 'radius 50 500 *canvas*)
  (add-spinbox-to-vbox vbox 'angle 30 360 *canvas*)
  (gtk-paned-add2 *paned* vbox)
  (gtk-widget-show-all *paned*))


;; Obwohl cl-cffi-gtk einen sehr umfangreichen Teil der GTK+
;; Bibliothek abdeckt fehlte ein fuer mich sehr wichtiger Teil, um
;; meine eigenen Daten in eine Cairo Surface zu laden. Dafuer sind
;; aber nur zwei Funktionen noetig deren Interface ich in diesem Patch
;; deklariere
;; https://github.com/plops/cl-cffi-gtk/commit/8eda1c404bcd6c10140103ea6332404cf9b357b0
;; Um den folgenden Code einfach zu halten, werde ich jedoch von
;; diesen Funktionen erstmal keinen Gebrauch machen.
