CFLAGS=`pkg-config gtk+-3.0 --cflags` -g -Wall -Wextra
LDFLAGS=`pkg-config gtk+-3.0 --libs`

custom-cell: custom-cell.c
