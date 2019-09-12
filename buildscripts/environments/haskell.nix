with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "haskell-environment";

  buildInputs = [
    stack
    haskellPackages.hoogle hlint
  ];
}
