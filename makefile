CFLAGS=`pkg-config gdk-3.0 gtk+-3.0 --cflags` -Wall -Wextra -ggdb --std=c99
LDFLAGS=`pkg-config gdk-3.0 gtk+-3.0 --libs` -lm

custom-widget: custom-widget.c

clean:
	rm custom-widget
