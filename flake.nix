{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    naersk.url = "github:nix-community/naersk/master";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils, naersk }:
    utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        naersk-lib = pkgs.callPackage naersk { };

        # Additional packages required to build Nickel on Darwin
        systemSpecificPkgs =
          if pkgs.stdenv.isDarwin then
            [
              pkgs.darwin.apple_sdk.frameworks.Security
              pkgs.darwin.libiconv
            ]
          else
            [ ];
      in
      {
        defaultPackage = naersk-lib.buildPackage ./.;
        devShell = with pkgs; mkShell {
          buildInputs = [
            # For Oclis itself
            cargo
            graphviz  # For generating the processing-pipeline infographic
            nickel  # For testing the Nickel code
            pre-commit
            rustc
            rustfmt
            rustPackages.clippy

            # For target languages PureScript and JavaScript
            nodejs_20
          ] ++ systemSpecificPkgs;
          RUST_SRC_PATH = rustPlatform.rustLibSrc;
        };
      });
}
