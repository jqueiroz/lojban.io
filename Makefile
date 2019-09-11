all: server docs

clean:
	rm -rf ./node_modules
	rm -rf ./.stack-work/dist
	rm -f ./static/scripts/*.js
	rm -f ./static/scripts/dictionaries/*.js
	rm -f ./static/style/*.css

deep-clean: clean
	#rm -rf ./.stack-work
	echo "deep clean is not yet enabled"

docs:
	./buildscripts/make-docs.sh

css:
	./buildscripts/make-css.sh

js:
	./buildscripts/make-javascript.sh

server:
	./buildscripts/make-server.sh
	./buildscripts/make-css.sh
	./buildscripts/make-javascript.sh
