(in-package :custom-class)

(defcfun ("g_type_instance_get_private" g-type-instance-get-private)
    :pointer
  (instance :pointer) ;; GTypeInstance
  (private-type g-type))

(defcfun ("g_signal_emit_by_name" g-signal-emit-by-name) :void
  (instance :pointer)
  (detailed-signal :string)
  &rest)

(defcfun ("g_signal_connect_data" %g-signal-connect-data) :ulong
  (instance :pointer)
  (detailed-signal :string)
  (c-handler :pointer)
  (data :pointer)
  (destroy-data :pointer) ;; fun(void*data,GClosure*closure)
  (connect-flags gtk::connect-flags))

(defcfun ("gtk_editable_get_type" gtk-editable-get-type) g-type)

(defcfun ("gtk_entry_set_text" gtk-entry-set-text) :void (entry :pointer) (text :string))

(defcfun ("g_type_check_instance_cast" g-type-check-instance-cast) :pointer (instance :pointer) (iface-type g-type))

(defcfun ("gtk_widget_get_type" gtk-widget-get-type) g-type)

