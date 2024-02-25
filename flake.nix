{
  inputs = {
    # nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    check-flake.url = "github:srid/check-flake";
    flake-root.url = "github:srid/flake-root";
    purescript-overlay.url = "github:thomashoneyman/purescript-overlay";
    purescript-overlay.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = inputs@{ self, nixpkgs, haskell-flake, check-flake, flake-root, flake-parts, purescript-overlay }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
        flake-root.flakeModule
        check-flake.flakeModule
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
          settings = {
            purescript-bridge.check = false;
            purescript-bridge.extraBuildDepends = [ pkgs.spago ];
            example.check = true;
            # https://community.flake.parts/haskell-flake/dependency#nixpkgs
            # aeson = { super, ... }:
            #   { custom = _: super.aeson_2_2_1_0; };
          };
          packages = {
            # servant.source = "0.20.1";
          };
          devShell = {
            enable = true;
            tools = haskellPackages: {
              # how to disable a package
              # haskell-language-server = null;
              inherit (haskellPackages) zlib stylish-haskell;
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
            pkgs.spago-unstable
            pkgs.purs-tidy-bin.purs-tidy-0_10_0
            pkgs.purs-backend-es
            pkgs.purescript-language-server
          ];
        };

        packages.default = self'.packages.example;
      };
    };
}
