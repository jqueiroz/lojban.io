with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "haskell-environment";

  nativeBuildInputs = [
  ];

  buildInputs = [
    stack
    haskellPackages.hoogle hlint
    (haskell-language-server.override {
      supportedGhcVersions = [ "810" ]; # lts-18.28
    })
  ];
}
