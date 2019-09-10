all: server docs

docs:
	./buildscripts/compile-docs.sh

less:
	./buildscripts/compile-less.sh

js:
	./buildscripts/make-javascript.sh

server:
	./buildscripts/compile-server.sh
	./buildscripts/compile-less.sh
