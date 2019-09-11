FROM johnjq/lojban-tool-dependencies:latest

####################### Configure the project #######################

# Copy project configuration files
COPY stack.yaml /lojto/stack.yaml
COPY lojto.cabal /lojto/lojto.cabal

# Copy source code
COPY courses /lojto/courses
COPY app /lojto/app
COPY src /lojto/src
COPY LICENSE /lojto/LICENSE

# Copy resources
COPY resources /lojto/resources

# Compile source code and documentation
RUN cd /lojto && stack haddock --no-haddock-deps --haddock-internal

# Move documentation
RUN mv /lojto/.stack-work/install/x86_64-linux-nix/*/*/doc /lojto/doc

# Copy audio files
COPY static/audio /lojto/static/audio

# Copy image files
COPY static/images /lojto/static/images

# Copy assets
COPY assets /lojto/assets

# Copy buildscripts
COPY buildscripts /lojto/buildscripts

# Generate stylesheet files
RUN cd /lojto && LOJBAN_TOOL_BYPASS_NIX=true ./buildscripts/compile-less.sh

# Generate javascript files
COPY package.json /lojto/package.json
COPY package-lock.json /lojto/package-lock.json
COPY gulpfile.js /lojto/gulpfile.js
RUN cd /lojto && npm install && LOJBAN_TOOL_BYPASS_NIX=true ./buildscripts/make-javascript.sh

####################### Expose ports #######################
EXPOSE 8000/tcp

####################### Default command #######################
CMD echo "hosts: files dns" > /etc/nsswitch.conf && (redis-server >/dev/null 2>/dev/null &) && cd /lojto && .stack-work/install/x86_64-linux-nix/*/*/bin/server
