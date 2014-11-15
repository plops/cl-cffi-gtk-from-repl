// http://scentric.net/tutorial/sec-custom-cell-renderers.html
#include <gtk/gtk.h>

int main(int argc, char**argv)
{
  GtkWidget *window, *label;
  gtk_init(&argc,&argv);
  window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(window),"hello world");
  gtk_widget_set_size_request(window,200,100);
  gtk_container_set_border_width(GTK_CONTAINER(window),10);

  g_signal_connect(G_OBJECT(window),"destroy",
		   G_CALLBACK({void _() { gtk_main_quit();  } (void (*)())_;}),NULL);
  g_signal_connect(G_OBJECT(window),"delete-event",
		   G_CALLBACK({gboolean _() { return FALSE;  } (void (*)())_;}),NULL);

  label = gtk_label_new("Hello World");
  gtk_label_set_selectable(GTK_LABEL(label),TRUE);

  gtk_container_add(GTK_CONTAINER(window),label);
  
  gtk_widget_show_all(window);
  

  gtk_main();
  
  return 0;
}
