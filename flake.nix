{
  inputs = {
    nixpkgs.url = "nixpkgs";
    systems.url = "github:nix-systems/default";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.url = "nixpkgs";
    };
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import inputs.systems;
      imports = with inputs; [
        haskell-flake.flakeModule
        treefmt-nix.flakeModule
      ];
      perSystem = {
        self',
        pkgs,
        config,
        ...
      }: {
        apps.default = self'.apps.aoc2023;
        packages.default = self'.packages.aoc2023;

        haskellProjects.default.autoWire = ["packages" "apps" "checks"];
        treefmt.config = {
          projectRootFile = "flake.nix";

          programs = {
            ormolu.enable = true;
            alejandra.enable = true;
            cabal-fmt.enable = true;
            hlint.enable = true;
          };
        };
        devShells.default = pkgs.mkShell {
          name = "aoc2023";
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          nativeBuildInputs = with pkgs; [just];
        };
      };
    };
}
