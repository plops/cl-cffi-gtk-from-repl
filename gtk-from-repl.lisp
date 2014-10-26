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


;; GTK+ ist eine C library die seine Inhalte auf eine
;; objektorientierte vorhaelt. Insbesonder sind die Widgets als CLOS
;; Klassen instanzierbar und ihre Parameter koennen entweder bei der
;; Instanzierung oder spaeter gesetzt werden. Die folgende Funktion
;; run-0 enthaelt minimalen Code um ein Fenster ohne weitere Widgets
;; zu erstellen.

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

;; Die Instanz window emittiert das Signal "destroy", wenn das Fenster
;; vom Window manager aus geschlossen wird. Mit der hier lambda
;; Funktion wird das Programm in diesem Fall abgebrochen.

;; In run-1 wird ein Button zum Fenster window hinzugefuegt.

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

;; Widgets koennen verschiedene Signale empfangen. Den besten
;; Ueberblick ueber moegliche Signal bekomme ich fuer gewoehnlich mit
;; Glade (siehe rechte untere Ecke im Screenshot).

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


;; In run-2 speichere ich das Objekt button in einer globalen Variable
;; *button*. Der Slime Inspector (C-c C-I) zeigt fuer dieses Objekt
;; den folgenden Inhalt an.

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


;; Ausserdem habe ich den Event Handler von window kopiert und an das
;; Signal "clicked" gehaengt. Im *inferior-lisp* Buffer von Emacs sehe
;; ich statt die erwarteten Textausgabe "button has been clicked".

;; Aus gegebenen Anlass moechte ich auf einen Fehler im
;; *inferior-lisp* Buffer hinweisen, den ich statt der gewuenschten
;; Textausgabe erhalten hatte, als meine definition des Signal Handler
;; so aus sah:

;; (g-signal-connect window "clicked"
;; 		      (lambda (widget)
;; 			(declare (ignorable widget))
;; 			(format t "button has been clicked~%")))


;; (sbcl:7507): GLib-GObject-WARNING **:
;; /var/tmp/portage/dev-libs/glib-2.40.0-r1/work/glib-2.40.0/gobject/gsignal.c:2362:
;; signal 'clicked' is invalid for instance '0x7fffe0005110' of type
;; 'GtkWindow'

;; In diesem Fall habe ich statt "button" "window" geschrieben und das
;; Fenster unterstuetzt eben kein signal "clicked".

;; An der Ausgabe des Inspectors fuer die Instanz *button* sehen wir den Slot label:
;; [ ]  LABEL                 = "test"

;; Ich moechte das Program zu einem Wuerfelprogramm umwandeln, dass
;; eine zufaellige Zahl zwischen 1 und 6 im Buttonlabel anzeigt.  Um
;; herauszufinden wie ich diese Modifikation einbauen kann druecke ich
;; M-. auf der Klassendefinition #<GOBJECT-CLASS GTK-BUTTON> in der
;; dritten Zeile im Inspector (alternativ geht auch gtk-button im
;; Aufruf von make-instance). Dadurch springe ich zur Definition der
;; Klasse im Quellcode von cl-cffi-gtk
;; ~/quicklisp/dists/quicklisp/software/cl-cffi-gtk-20141006-git/gtk/gtk.button.lisp

;; Die Stelle sieht so aus:

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

;; Nach einigem Experimentieren sehe ich dass gtk-button-label die
;; Method ist, die ich brauche um den Labeltext auszulesen und zu
;; aendern:
    
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

;; Die Funktion run-3 ist damit die erste halbwegs vernuenftige GTK Applikation.


;; Jetzt moechte ich eine interessantere Applikation bauen.  Wie dem
;; Screenshot zu entnehmen ist, soll das Fenster auf der linken Seite
;; einen Cairo Canvas zeigen. Darin male ich einen Kreis dessen
;; Mittelpunktskoordinaten und Radius durch GUI Elemente auf der
;; rechten Seite des Fensters eingestellt werden koennen.

;; Ein gtk-paned hat zwei Seiten und einen variablen schieber
;; dazwischen. Ich nutze dieses Element als hoechstes Element unter
;; dem Fenster um die Widgets anzuordnen. Damit ich spaeter auf diese
;; Widgets zugreifen kann. Speichere ich die Paned Instanz in der
;; globalen Variable *paned*. Weiterhin ist es nuetzlich eine globale
;; Variable mit dem Cairo Canvas zu haben, so dass neue
;; Kontrollwidgets sehr einfach einen redraw erzwingen koennen.

(defparameter *paned* nil)
(defparameter *canvas* nil)

;; Der Cairo Canvas ist ein Kapitel fuer sich und wird gut in
;; folgendem Tutorial zu cl-cffi-gtk erklaert:
;; http://www.crategus.com/books/cl-gtk/gtk-tutorial_16.html#SEC172

;; Im wesentlichen muss man nur eine draw Funktion definieren, die mit
;; einzelnen Funktionsaufrufen eine Farbe setzen und eine Kurve
;; malen. Die Semantik aehnelt sehr der von Postscript.

;; Die folgende einfache Funktion malt einen violetten Kreis und eine
;; rote Linie auf den Canvas.

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

;; Zu bemerken ist, dass ich die Funktion in einer globalen Variable
;; *draw-canvas* speichere. Spaeter im "draw" Signal handler fuer den
;; Canvas rufe ich mit funcall die darin gespeicherte Funktion
;; auf. Das hat den Vorteil, dass ich draw-canvas zur Laufzeit neu
;; definieren kann. Wenn es beim Bauen Fehler gibt, dann wird
;; (defparameter *draw-canvas* #'draw-canvas) nicht aufgerufen und die
;; alte Definition bleibt bestehen. Erst wenn die Compilation gelingt
;; wird *draw-canvas* ersetzt und die neue Funktion malt in den
;; Canvas.

;; Die Koordinaten radius, angle, x und y sollen von der GUI entnommen
;; werden. Da die aber bisher noch nicht definiert ist, schreibe ich
;; erstmal folgende Platzhalterfunktion. 

#+nil
(defun spin-button-value (name paned)
  "Return the adjustment value of the spin-button that is labeled with NAME."
  nil)

;; Da spin-button-value zunaechst immer nil liefert, bewirkt dass das
;; "or" in draw-canvas auf die dahinterstehnden Zahlenkonstanten als
;; default Werte ausweicht. Ganz zum Schluss nutze werde mich mit
;; Hilfe der GTK+ Methoden durch alle GUI Elemente zu den aktuellen
;; Werten zu hangeln. Dabei wird es sich als sehr hilfreich erweisen,
;; dass die Objekte interaktiv im REPL und dem Slime Inspector
;; analysiert werden koennen. Bei Programmierung mit C waere eine
;; derartige Vorgehensweise undenkbar.

;; Die Funktion run-4 baut nun wie vorher das GUI Fenster auf. Als
;; erstes wird ein Hauptfenster (top-level window) erstellt. Darin ein
;; Paned Objekt, dass das Fenster vertikal in zwei Teile teilt. Links
;; kommt der Cairo Canvas hin. Dessen Groesse wird auf 1024x1024
;; gesetzt und ich packe ein scrolled-window drumherum um scrollbars
;; zu haben. Auf die rechte Seite des Paned kommt eine Box mit
;; vertikal angeordneten Spinboxen. 

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

;; Die Spinbox ist meineserachtens das beste Widget um mein
;; Benutzerinterface zu repraesentiern. Sie bestehen aus einer Zahl
;; und rechts daneben sind ein Pfeil hoch und runter, um den Wert per
;; Mausklick anzupassen. Alternativ kann der Wert direkt per
;; Texteingabe, durch Tastatendruck (Pfeil- oder Bildtasten)
;; veraendert werden. Der Wertebereich und die den Tastendruecken
;; entsprechenden diskreten Stufen werden durch die Klasse GTK
;; Adjustment repraesentiert. Wenn sich der im Adjustment gespeicherte
;; Wert aendert, sende ich mit gtk-widget-queue-draw einen Nachricht
;; an den Canvas, dass dieser sich neu zeichnen soll.

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
(spin-button-value 'ypos *paned*) ;; => 75.0


#+nil
(run-4)


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
