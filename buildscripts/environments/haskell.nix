with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "haskell-environment";

  nativeBuildInputs = [
    #pkg-config zlib # # Fixes error: Cabal-... The pkg-config package 'zlib' is required but it could not be found
  ];

  buildInputs = [
    stack
    haskellPackages.hoogle hlint
    (haskell-language-server.override {
      supportedGhcVersions = [ "947" ]; # lts-21.21
    })
  ];
}
