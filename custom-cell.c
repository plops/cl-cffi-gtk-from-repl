// http://scentric.net/tutorial/sec-custom-cell-renderers.html
#include <gtk/gtk.h>

#include <glib.h>
#include <glib-object.h>
#include <gtk/gtkentry.h>

// each new object needs 5 macros
#define MY_IP_ADDRESS_TYPE (my_ip_address_get_type())
#define MY_IP_ADDRESS(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), MY_IP_ADDRESS_TYPE, MyIPAddress))
#define MY_IP_ADDRESS_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST((klass), MY_IP_ADDRESS_TYPE, MyIPAddressClass))
#define IS_MY_IP_ADDRESS(obj) (G_TYPE_CHECK_INSTANCE_TYPE((obj), MY_IP_ADDRESS_TYPE))
#define IS_MY_IP_ADDRESS_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE((klass), MY_IP_ADDRESS_TYPE))

typedef struct _MyIPAddress MyIPAddress;
typedef struct _MyIPAdressClass MyIPAdressClass;

struct _MyIPAdress {
  GtkEntry entry;
};

struct _MyIPAdressClass {
  GtkEntryClass parent_class;
  void (*ip_changed)(MyIPAddress *ipaddress);
};


GType my_ip_address_get_type (void) G_GNUC_CONST;
GtkWidget* my_ip_address_new(void);
gchar* my_ip_address_get_address(MyIPAddress *ipaddress);
void my_ip_address_set_address(MyIPAddress*ipaddress,gint address[4]);

#include <gdk/gdkkeysyms.h>
#include <stdlib.h>
#include <math.h>

#define MY_IP_ADDRESS_GET_PRIVATE(obj) (G_TYPE_INSTANCE_GET_PRIVATE((obj),MY_IP_ADDRESS_TYPE,MyIPAddressPrivate))

// holds private properties of the object, which are unique to each
// instance
typedef struct _MyIPAddressPrivate MyIPAddressPrivate;
struct _MyIPAddressPrivate {
  guint address[4];
};

enum {
  CHANGED_SIGNAL, // will be emitted when user or program changes content of IP address
  LAST_SIGNAL };

enum {
  PROP_0,
  PROP_IP1,
  PROP_IP2,
  PROP_IP3,
  PROP_IP4};

static guint my_ip_address_signals[LAST_SIGNAL]={0};

// return a numercial value that is unique to the registered type
GType my_ip_address_get_type(void) 
{
  static GType entry_type = 0;

  if(!entry_type){
    static const GTypeInfo entry_info = {
      sizeof(MyIPAddressClass), // size of the structure
      NULL, // base_init reallocate all dynamic class members copied from base class
      NULL, // base_finalize finalize things done by base_init
      (GClassInitFunc)my_ip_address_class_init,
      // class_init fill virtual functions and register signals
      NULL, // class_finalize, barely needed because base... deals with dynamically allocated resources
      NULL, // class_data a pointer passed to class_init and class_finalize
      sizeof(MyIPAddress), // instance_size 
      0, // n_preallocs (ignored since glib 2.1)
      (GInstanceInitFunc) my_ip_address_init
      // instance_init, here it connects signals and packs widget
      // value_table only used when creating fundamental types
    };
    entry_type = g_type_register_static(GTK_TYPE_ENTRY, "MyIPAddress", &entry_info, 0);
  }
  return entry_type;
}


int main(int argc, char**argv)
{
  GtkWidget *window, *label;
  gtk_init(&argc,&argv);
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window),"hello world");
  gtk_widget_set_size_request(window,200,100);
  gtk_container_set_border_width(GTK_CONTAINER(window),10);

  g_signal_connect(G_OBJECT(window),"destroy",
		   G_CALLBACK({void _(GtkWidget*window,gpointer data) {
			 gtk_main_quit();  } (void (*)(GtkWidget*window,gpointer data))_;}),NULL);
  g_signal_connect(G_OBJECT(window),"delete-event",
		   G_CALLBACK({gboolean _(GtkWidget*window,GdkEvent*event,gpointer data) {
			 return FALSE;  } (gboolean (*)(GtkWidget*window,GdkEvent*event,gpointer data))_;}),NULL);

  label = gtk_label_new("Hello World");
  gtk_label_set_selectable(GTK_LABEL(label),TRUE);

  gtk_container_add(GTK_CONTAINER(window),label);
  
  gtk_widget_show_all(window);
  

  gtk_main();
  
  return 0;
}
