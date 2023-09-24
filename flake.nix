{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    flake-root.url = "github:srid/flake-root";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, haskell-flake, flake-root, flake-parts, purescript-overlay }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
        flake-root.flakeModule
      ];

      perSystem = { self', pkgs, system, config,... }: {

        # https://flake.parts/overlays#consuming-an-overlay
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            purescript-overlay.overlays.default
          ];
        };

        haskellProjects.default = {
          basePackages = pkgs.haskellPackages;
          devShell = {
            enable = true;
            mkShellArgs = {
              shellHook = ''
                export LD_LIBRARY_PATH=${pkgs.zlib.out}/lib:LD_LIBRARY_PATH
              '';
            };
            tools = haskellPackages: {
              inherit (haskellPackages)
                zlib;
            };
            hlsCheck.enable = false;
          };

          # exclude devShell, fixes duplicate definition
          autoWire = [ "packages" "apps" "checks" ];
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [
            config.haskellProjects.default.outputs.devShell
          ];
          buildInputs = [
            pkgs.hello
            pkgs.purs
            pkgs.spago
            pkgs.purs-tidy-bin.purs-tidy-0_10_0
            pkgs.purs-backend-es
            pkgs.purescript-language-server
          ];
        };

        packages.default = self'.packages.example;
      };
    };
}
