all: server docs

clean:
	rm -rf ./.stack-work/dist
	rm -f ./static/scripts/*.js
	rm -f ./static/scripts/dictionaries/*.js
	rm -f ./static/style/*.css

deep-clean: clean
	#rm -rf ./node_modules
	#rm -rf ./.stack-work
	echo "deep clean is not yet enabled"

docs:
	./buildscripts/compile-docs.sh

less:
	./buildscripts/compile-less.sh

js:
	./buildscripts/make-javascript.sh

server:
	./buildscripts/compile-server.sh
	./buildscripts/compile-less.sh
	./buildscripts/make-javascript.sh
