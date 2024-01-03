{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/haskell-updates";
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
            example.check = true;
            # https://community.flake.parts/haskell-flake/dependency#nixpkgs
            floskell = { super, ... }:
              { custom = _: super.floskell_0_11_0; };
            aeson = { super, ... }:
              { custom = _: super.aeson_2_2_1_0; };
            aeson-pretty = { super, ... }:
              { custom = _: super.aeson-pretty_0_8_10; };
          };
          packages = {
            attoparsec-aeson.source = "2.2.0.1";
            servant.source = "0.20.1";
            servant-server.source = "0.20";
            th-abstraction.source = "0.5.0.0";
            http-conduit.source = "2.3.8.3";
          };
          devShell = {
            enable = true;
            mkShellArgs = {
              # shellHook = ''
              #   export LD_LIBRARY_PATH=${pkgs.zlib.out}/lib:LD_LIBRARY_PATH
              # '';
            };
            tools = haskellPackages: {
              # disable until haskell-language-server compatible with Aeson 2.2
              haskell-language-server = null;
              inherit (haskellPackages) zlib;
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
