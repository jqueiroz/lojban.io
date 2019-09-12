with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "lint";

  buildInputs = [
    pkgs.hlint pkgs.jq nodePackages.prettier
  ];
}
