CFLAGS := -g -Wall -Wpedantic -fprofile-arcs -ftest-coverage
LDFLAGS := --coverage

all: rook

demo: rook
	./rook hello.rk

test: rook
	./test t/*
cover:
	lcov --directory . \
	     --base-directory . \
	     --capture -o lcov.info
	genhtml -o coverage lcov.info
coverage: test cover
copycov: coverage
	rm -rf /vagrant/coverage
	cp -a coverage /vagrant

clean:
	rm -rf coverage/
	rm -f *.gc??
	rm -f lcov.info rook *.o

rook: rook.o

.PHONY: demo test cover coverage copycov clean
