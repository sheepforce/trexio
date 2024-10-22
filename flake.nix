{
  description = "TREX I/O library";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let overlay = import ./nix/overlay.nix;
    in flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ overlay ];
        };
      in
      {
        packages = {
          inherit (pkgs) trexio;
          trexio-hs = pkgs.haskellPackages.trexio;
        };

        devShells = {
          haskell = pkgs.haskellPackages.shellFor {
            withHoogle = true;
            packages = p: [ p.trexio ];
            buildInputs = with pkgs; [
              haskell-language-server
              haskellPackages.fourmolu
              hlint
            ];
          };
        };
      }
    );
}
