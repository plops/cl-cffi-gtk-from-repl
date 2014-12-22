;;(cc-flags #.(asdf:run-shell-command "pkg-config gtk+-3.0 --cflags"))
(in-package :custom-class)
(cc-flags "-I/usr/include/gtk-3.0" "-I/usr/include/at-spi2-atk/2.0" "-I/usr/include/gtk-3.0" "-I/usr/include/gio-unix-2.0/" "-I/usr/include/cairo" "-I/usr/include/pango-1.0" "-I/usr/include/harfbuzz" "-I/usr/include/pango-1.0" "-I/usr/include/atk-1.0" "-I/usr/include/cairo" "-I/usr/include/pixman-1" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/gdk-pixbuf-2.0" "-I/usr/include/libpng16" "-I/usr/include/glib-2.0" "-I/usr/lib64/glib-2.0/include" "-I/home/martin/cl-cffi-gtk-from-repl")
(include "gdk/gdk.h")
(include "gtk/gtk.h")
(include "cellrenderer_priv.h")

(cstruct %gobject-class "GObjectClass"
	 (set-property "set_property" :type :pointer)
	 (get-property "get_property" :type :pointer)
	 (dispose "dispose" :type :pointer)
	 (finalize "finalize" :type :pointer) ;;void       (*finalize)                (GObject        *object);
	 )
(cstruct %gtk-entry "GtkEntry")
(cstruct %gtk-entry-class "GtkEntryClass")
(cstruct %gtk-cell-renderer-private "GtkCellRendererPrivate"
	 (xalign "xalign" :type :float)
	 (yalign "yalign" :type :float)
	 (width "width" :type :int)
	 (height "height" :type :int)
	 (xpad "xpad" :type :uint16)
	 (ypad "ypad" :type :uint16)
	 ;; fixme i don't know how to get access to the bitmask
	 ;; containing mode
	 )
(cstruct %gtk-cell-renderer "GtkCellRenderer"
	 ;(parent-instance ) ;; GInitiallyUnowned parent_instance;
	 (priv "priv" :type :pointer) ;; GtkCellRendererPrivate
	 )

(cstruct %gtk-cell-renderer-class "GtkCellRendererClass"
	 (get-size "get_size" :type :pointer)
	 (render "render" :type :pointer))
(cstruct %gtk-cell-renderer-spin "GtkCellRendererSpin")
(cstruct %gtk-cell-renderer-spin-class "GtkCellRendererSpinClass")
(cenum (%gdk-event-type :define-constants t)
       ((:nothing "GDK_NOTHING"))
       ((:key-press "GDK_KEY_PRESS"))
       ((:key-release "GDK_KEY_RELEASE")))
(cstruct %gdk-event-key "GdkEventKey"
	 (keyval "keyval" :type :uint)
	 (string "string" :type :string)
	 (send-event "send_event" :type :int8)
	 (window "window" :type :pointer)
	 (type "type" :type :int) ;%gdk-event-type
	 (time "time" :type :uint32)
	 (state "state" :type :uint)
	 (length "length" :type :int)
	 (hardware-keycode "hardware_keycode" :type :uint16)
	 (group "group" :type :uint8)
	 ;; (is-modifier "is_modifier" :type :uint) ; this is a bit field
	 )
