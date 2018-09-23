CFLAGS := -g -Wall -Wpedantic -fprofile-arcs -ftest-coverage
LDFLAGS := --coverage

all: rook0

demo: rook0
	./rook0 hello.rk

test: rook0
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
	rm -f lcov.info rook rook0 *.o

rook0: rook0.o

.PHONY: all demo test cover coverage copycov clean
