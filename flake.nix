{
  description = "Development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    system = "x86_64-linux";
    pkgs = import nixpkgs { inherit system; };
  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = with pkgs; [
        gnumake
        python3
        pandoc
        haskell.packages.ghc9103.ghc
        haskell.packages.ghc9103.haskell-language-server
      ];
    };
  };
}
