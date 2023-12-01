all: server docs

clean:
	rm -f ./*.prof
	rm -f ./assets/package-lock.json
	rm -rf ./assets/node_modules
	rm -f ./stack.yaml.lock
	rm -rf ./.stack-work/dist
	rm -f ./static/scripts/*.js
	rm -f ./static/scripts/dictionaries/*.js
	rm -f ./static/style/*.css

deep-clean: clean
	rm -rf ./.stack-work

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

server-minus-js:
	./buildscripts/make-server.sh
	./buildscripts/make-css.sh

tests:
	./verify-tests.sh

lint:
	./verify-lint.sh
