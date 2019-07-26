with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "less-environment";

  buildInputs = [
    pkgs.lessc
  ];
}
