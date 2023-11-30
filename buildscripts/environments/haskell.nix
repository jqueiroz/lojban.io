with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "haskell-environment";

  nativeBuildInputs = [
    pkg-config zlib
  ];

  buildInputs = [
    stack
    pkgs.haskellPackages.zlib # Fixes error: Cabal-... The pkg-config package 'zlib' is required but it could not be found
    haskellPackages.hoogle hlint
    (haskell-language-server.override {
      #supportedGhcVersions = [ "810" ]; # lts-18.28
    })
  ];
}
