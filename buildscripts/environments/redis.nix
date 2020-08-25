with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "redis";

  buildInputs = [
    pkgs.redis
  ];
}
