.POSIX:
.SUFFIXES:
.PHONY: all clean check install uninstall

CFLAGS = -ansi -pedantic -Wall -Wextra
LDFLAGS =
CC = cc
PREFIX = /usr/local

all: bin/lngrd

clean:
	rm -f bin/lngrd obj/main.o

check: bin/lngrd
	cd test && ./check ../bin/lngrd

install: bin/lngrd
	cp bin/lngrd $(DESTDIR)$(PREFIX)/bin/ && cp src/lngrd.h $(DESTDIR)$(PREFIX)/include/

uninstall:
	rm -f $(DESTDIR)$(PREFIX)/bin/lngrd && rm -f $(DESTDIR)$(PREFIX)/include/lngrd.h

bin/lngrd: bin obj obj/main.o
	$(CC) $(LDFLAGS) -o bin/lngrd obj/main.o

obj/main.o: src/main.c src/lngrd.h
	$(CC) $(CFLAGS) -c src/main.c -o obj/main.o

bin:
	mkdir bin

obj:
	mkdir obj
