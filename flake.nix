{
  description = "Nix Flake for OCaml";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url =  "github:nixos/nixpkgs/nixos-24.11";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          name = "ocaml shell";
          packages = with pkgs;
            [

              ocaml
              dune_3

            ]
            ++ (with ocamlPackages; [
              # Ocaml packages
              ocaml-lsp
              ocamlformat
              findlib

              utop

              menhir
              menhirLib
              ppx_compare

            ]);
        };
      }
    );
}
