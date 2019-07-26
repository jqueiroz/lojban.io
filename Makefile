docs:
	./compile-docs.sh

less:
	./buildscripts/compile-less.sh

server: less
	./compile-server.sh

all: docs server
