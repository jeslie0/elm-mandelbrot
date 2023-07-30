{
  description = "Generating a Mandelbrot set in Elm";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    mkElmDerivation.url = "github:jeslie0/mkElmDerivation";
  };

  outputs = { self, nixpkgs, mkElmDerivation, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          overlays = [ mkElmDerivation.overlays.default ];
          inherit system;
        };
        elmPackageName = "mandelbrot";
        elmPackage = pkgs.mkElmDerivation {
            name = elmPackageName;
            src = ./.;
            outputJavaScript = true;
          };
      in
        {
          packages.default = pkgs.stdenv.mkDerivation {
            pname = elmPackageName;
            version = "0.1.0";
            src = ./public;
            buildInputs = [elmPackage];
            buildPhase = ''
                         cp ${elmPackage}/Main.min.js main.min.js
                         '';
            installPhase = ''
                         mkdir $out
                         cp * $out
                         '';
          };


          devShell = pkgs.mkShell {
            inputsFrom = [ elmPackage ];
            packages = with pkgs;
              [ elmPackages.elm-language-server
                elmPackages.elm-live
                elmPackages.elm-format
              ];
          };
        }
    );
}
