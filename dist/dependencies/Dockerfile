FROM nixorg/nix:latest

####################### Install dependencies #######################

RUN mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf
RUN nix-channel --add https://nixos.org/channels/nixos-19.03 nixpkgs
RUN nix-channel --update
RUN nix-env -iA nixpkgs.stack nixpkgs.coreutils nixpkgs.lessc nixpkgs.findutils nixpkgs.redis

####################### Configure the project #######################

# Copy project configuration files
COPY stack.yaml /lojto/stack.yaml
COPY lojto.cabal /lojto/lojto.cabal

# Setup GHC
RUN cd /lojto && stack setup

# Install dependencies
COPY libs /lojto/libs
RUN cd /lojto && stack install --only-dependencies
