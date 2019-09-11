with import <nixpkgs> {};

stdenv.mkDerivation {
  name = "javascript";

  buildInputs = [
    nodejs-10_x
    #nodePackages.typescript
    #nodePackages.gulp
  ];
}
