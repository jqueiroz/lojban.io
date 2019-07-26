docs:
	./compile-docs.sh

less:
	./compile-less.sh

server: less
	./compile-server.sh

all: docs server
