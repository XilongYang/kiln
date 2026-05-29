{
  description = "Kiln builder";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
  let
    version = "0.1.0";
    systems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f system);
    srcFilteredFor = pkgs:
      let
        fs = pkgs.lib.fileset;
      in
      fs.toSource {
        root = ./.;
        fileset = fs.unions [
          ./Src
          ./LICENSE
          ./README.md
        ];
      };
    mkKilnUnwrapped = pkgs:
      pkgs.stdenv.mkDerivation {
        pname = "kiln";
        inherit version;
        src = srcFilteredFor pkgs;

        nativeBuildInputs = [ pkgs.haskell.packages.ghc9103.ghc ];

        buildPhase = ''
          runHook preBuild
          ghc -O2 -iSrc -o kiln Src/Main.hs
          runHook postBuild
        '';

        installPhase = ''
          runHook preInstall
          mkdir -p $out/bin
          cp kiln $out/bin/kiln
          runHook postInstall
        '';
      };
    mkKiln = pkgs:
      let
        kilnUnwrapped = mkKilnUnwrapped pkgs;
        pyWithFontTools = pkgs.python314.withPackages (ps: with ps; [
          fonttools
          brotli
        ]);
        runtimeDeps = with pkgs; [
          coreutils
          pandoc
          pyWithFontTools
        ];
      in
      pkgs.symlinkJoin {
        name = "kiln-${version}";
        paths = [ kilnUnwrapped ];
        nativeBuildInputs = [ pkgs.makeWrapper ];
        postBuild = ''
          wrapProgram $out/bin/kiln --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
        '';
      };
  in {
    overlays.default = final: prev: {
      kiln = mkKiln final;
      kiln-unwrapped = mkKilnUnwrapped final;
    };

    packages = forAllSystems (system:
      let
        pkgs = import nixpkgs { inherit system; };
        kiln = mkKiln pkgs;
        kilnUnwrapped = mkKilnUnwrapped pkgs;
      in {
        default = kiln;
        kiln = kiln;
        kiln-unwrapped = kilnUnwrapped;
      });

    apps = forAllSystems (system:
      let pkg = self.packages.${system}.kiln;
      in {
        default = {
          type = "app";
          program = "${pkg}/bin/kiln";
        };
        kiln = {
          type = "app";
          program = "${pkg}/bin/kiln";
        };
      });

    devShells = forAllSystems (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        default = pkgs.mkShell {
          packages = with pkgs;
          let
            pyWithFontTools = python314.withPackages (ps: with ps; [
              fonttools
              brotli
            ]);
          in [
            gnumake
            haskell.packages.ghc9103.ghc
            haskell.packages.ghc9103.haskell-language-server
            pandoc
            pyWithFontTools
          ];
        };
      });
  };
}
