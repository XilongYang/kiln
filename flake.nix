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
        # Core
        gnumake
        haskell.packages.ghc9103.ghc
        pandoc
        python314Packages.fonttools
        python314Packages.brotli

        # Dev
        haskell.packages.ghc9103.haskell-language-server
        vscode-langservers-extracted
        typescript-language-server
      ];
    };
  };
}
