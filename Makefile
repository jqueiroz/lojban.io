all: server docs

docs:
	./buildscripts/compile-docs.sh

less:
	./buildscripts/compile-less.sh

server: less
	./buildscripts/compile-server.sh
