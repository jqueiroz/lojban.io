with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "javascript";

  buildInputs = [
    nodePackages.typescript
    nodePackages.gulp
  ];
}
