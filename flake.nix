{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      # systems = ["x86_64-linux" "x86_64-darwin"];
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem = { self', pkgs, ... }: {

        haskellProjects.default = {

          packages = {
            purescript-bridge.root = ./.;
            example.root = ./example;
          };

          basePackages = pkgs.haskellPackages;

          # Dependency overrides go here. See https://haskell.flake.page/dependency
          # source-overrides = { };
          # overrides = self: super: { };

          devShell = {
            enable = true;

            mkShellArgs = {
              shellHook = ''
                export LD_LIBRARY_PATH=${pkgs.zlib.out}/lib:LD_LIBRARY_PATH
              '';
            };

            tools = hp: {
              inherit (pkgs)
                purescript
                spago
                zlib;
            };

            hlsCheck.enable = false;
          };
        };

        packages.default = self'.packages.example;
      };
    };
}
