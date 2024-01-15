with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "dnscontrol";

  buildInputs = [
    pkgs.dnscontrol
    pkgs.python3
  ];
}
