FROM nixorg/nix:latest

####################### Install dependencies #######################

RUN mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf
RUN nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
RUN nix-channel --update
RUN nix-env -iA nixpkgs.stack nixpkgs.coreutils nixpkgs.lessc nixpkgs.findutils

####################### Configure the project #######################

# Copy project configuration files
COPY stack.yaml /lojto/stack.yaml
COPY lojto.cabal /lojto/lojto.cabal

# Setup GHC
RUN cd /lojto && stack setup

# Install dependencies
COPY libs /lojto/libs
RUN cd /lojto && stack install --only-dependencies

# Copy source code
COPY courses /lojto/courses
COPY src /lojto/src
COPY LICENSE /lojto/LICENSE

# Compile source code
RUN cd /lojto && stack build

# Copy additional resources
COPY resources /lojto/resources
COPY static /lojto/static

# Compile stylesheet files
COPY compile-less.sh /lojto/compile-less.sh
RUN cd /lojto && ./compile-less.sh

####################### Expose ports #######################
EXPOSE 8000/tcp

####################### Default command #######################
CMD cd /lojto && .stack-work/install/x86_64-linux-nix/lts-*/*/bin/lojto
