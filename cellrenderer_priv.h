struct _GtkCellRendererPrivate
{
  gfloat xalign;
  gfloat yalign;

  gint width;
  gint height;

  guint16 xpad;
  guint16 ypad;

  guint mode                : 2;
  guint visible             : 1;
  guint is_expander         : 1;
  guint is_expanded         : 1;
  guint cell_background_set : 1;
  guint sensitive           : 1;
  guint editing             : 1;

  GdkRGBA cell_background;
};

struct _GtkCellRendererClassPrivate
{
  GType accessible_type;
};
