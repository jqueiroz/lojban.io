with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "css-environment";

  buildInputs = [
    pkgs.lessc
  ];
}
