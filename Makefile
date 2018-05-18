CFLAGS := -g -Wall -Wpedantic

test: rook
	./rook hello.rk

rook: rook.o
