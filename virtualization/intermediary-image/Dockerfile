FROM nixos/nix:latest

####################### Install dependencies #######################

RUN echo 2023-11-27
RUN mkdir -p /etc/nix && echo 'sandbox = false' > /etc/nix/nix.conf
RUN nix-channel --add https://nixos.org/channels/nixos-23.11 nixpkgs
RUN nix-channel --update
RUN nix-env -iA nixpkgs.bash nixpkgs.stack nixpkgs.coreutils nixpkgs.lessc nixpkgs.findutils nixpkgs.redis nixpkgs.nodejs

####################### Set locale #######################

ENV \
  LANG=en_US.UTF-8 \
  LC_ALL=en_US.UTF-8 \
  LC_CTYPE=en_US.UTF-8

####################### Configure the project #######################

# Copy project configuration files
COPY stack.yaml /lojbanios/stack.yaml
COPY lojbanios.cabal /lojbanios/lojbanios.cabal

# Copy libraries
COPY haskell/libs /lojbanios/haskell/libs

# Setup GHC
RUN cd /lojbanios && stack setup

# Install dependencies
RUN cd /lojbanios && stack install --only-dependencies
