FROM johnjq/lojbanios-dependencies:latest

####################### Configure the project #######################

# Copy project configuration files
COPY stack.yaml /lojbanios/stack.yaml
COPY lojbanios.cabal /lojbanios/lojbanios.cabal

# Copy source code
COPY haskell /lojbanios/haskell
COPY LICENSE /lojbanios/LICENSE

# Copy resources
COPY resources /lojbanios/resources

# Compile source code and documentation
RUN cd /lojbanios && stack haddock --no-haddock-deps --haddock-internal

# Move documentation
RUN mv /lojbanios/.stack-work/install/x86_64-linux-nix/*/*/doc /lojbanios/documentation

# Copy audio files
COPY static/audio /lojbanios/static/audio

# Copy image files
COPY static/images /lojbanios/static/images

# Copy pwa files
COPY static/pwa /lojbanios/static/pwa

# Copy assets
COPY assets /lojbanios/assets

# Copy buildscripts
COPY buildscripts /lojbanios/buildscripts

# Generate stylesheet files
RUN cd /lojbanios && LOJBANIOS_BYPASS_NIX=true ./buildscripts/make-css.sh

# Generate javascript files
RUN cd /lojbanios && LOJBANIOS_BYPASS_NIX=true ./buildscripts/make-javascript.sh

####################### Expose ports #######################
EXPOSE 8000/tcp

####################### Default command #######################
CMD echo "hosts: files dns" > /etc/nsswitch.conf && (redis-server >/dev/null 2>/dev/null &) && cd /lojbanios && sleep 3 && .stack-work/install/x86_64-linux-nix/*/*/bin/server
