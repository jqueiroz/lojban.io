with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "haskell-environment";

  nativeBuildInputs = [
  ];

  buildInputs = [
    stack
    haskellPackages.hoogle hlint
    (haskellPackages.haskell-language-server.overrideAttrs (oldAttrs: {
      #supportedGhcVersions = [ ];
      supportedGhcVersions = [ "884" ]; # lts-16.31
    }))
  ];
}
