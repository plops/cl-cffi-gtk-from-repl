;;(cc-flags #.(asdf:run-shell-command "pkg-config gtk+-3.0 --cflags"))
(in-package :custom-class)
(cc-flags "-I/usr/include/gtk-3.0" "-I/usr/include/at-spi2-atk/2.0" "-I/usr/include/gtk-3.0" "-I/usr/include/gio-unix-2.0/" "-I/usr/include/cairo" "-I/usr/include/pango-1.0" "-I/usr/include/harfbuzz" "-I/usr/include/pango-1.0" "-I/usr/include/atk-1.0" "-I/usr/include/cairo" "-I/usr/include/pixman-1" "-I/usr/include/freetype2" "-I/usr/include/libdrm" "-I/usr/include/gdk-pixbuf-2.0" "-I/usr/include/libpng16" "-I/usr/include/glib-2.0" "-I/usr/lib64/glib-2.0/include" "-I/home/martin/cl-cffi-gtk/")
(include "gdk/gdk.h")
(include "gtk/gtk.h")
(include "cellrenderer_priv.h")

(cstruct %gobject-class "GObjectClass"
	 (set-property "set_property" :type :pointer)
	 (get-property "get_property" :type :pointer))
(cstruct %gtk-entry "GtkEntry")
(cstruct %gtk-entry-class "GtkEntryClass")
(cstruct %gtk-cell-renderer "GtkCellRenderer"
	 ())
(cstruct %gtk-cell-renderer-class "GtkCellRendererClass")
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
