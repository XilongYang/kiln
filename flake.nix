{
  description = "Kiln builder";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      version = "0.1.1";
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      mkKiln = pkgs:
        let
          hpkgs = pkgs.haskell.packages.ghc9103;
          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [ ./app ./src ./kiln.cabal ./LICENSE ./README.md ];
          };
          cabalDrv = hpkgs.callCabal2nix "kiln" src { };
          runtimeDeps = with pkgs; [
            coreutils
            pandoc
            (python314.withPackages (ps: [ ps.fonttools ps.brotli ]))
          ];
          kilnUnwrapped = pkgs.haskell.lib.justStaticExecutables cabalDrv;
          kiln = pkgs.symlinkJoin {
            name = "kiln-${version}";
            paths = [ kilnUnwrapped ];
            nativeBuildInputs = [ pkgs.makeWrapper ];
            postBuild = ''
              wrapProgram $out/bin/kiln --prefix PATH : ${pkgs.lib.makeBinPath runtimeDeps}
            '';
          };
          devShell = cabalDrv.env.overrideAttrs (old: {
            nativeBuildInputs = (old.nativeBuildInputs or [ ])
              ++ [ pkgs.cabal-install hpkgs.haskell-language-server ]
              ++ runtimeDeps;
          });
        in
        { inherit kiln kilnUnwrapped devShell; };

      perSystem = nixpkgs.lib.genAttrs systems
        (system: mkKiln (import nixpkgs { inherit system; }));
    in
    {
      overlays.default = final: prev:
        let built = mkKiln final; in
        { inherit (built) kiln; "kiln-unwrapped" = built.kilnUnwrapped; };

      packages = nixpkgs.lib.mapAttrs
        (_: built: { default = built.kiln; inherit (built) kiln; "kiln-unwrapped" = built.kilnUnwrapped; })
        perSystem;

      apps = nixpkgs.lib.mapAttrs
        (_: built: {
          default = { type = "app"; program = "${built.kiln}/bin/kiln"; };
          kiln = { type = "app"; program = "${built.kiln}/bin/kiln"; };
        })
        perSystem;

      devShells = nixpkgs.lib.mapAttrs (_: built: { default = built.devShell; }) perSystem;
    };
}
