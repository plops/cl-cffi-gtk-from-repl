CFLAGS=`pkg-config gdk-3.0 gtk+-3.0 --cflags` -g -Wall -Wextra
LDFLAGS=`pkg-config gdk-3.0 gtk+-3.0 --libs`

custom-cell: custom-cell.c
