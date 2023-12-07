{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };

  outputs = inputs:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux"];
      imports = [inputs.haskell-flake.flakeModule];
      perSystem = {
        pkgs,
        config,
        ...
      }: {
        haskellProjects.default = {
          autoWire = ["packages" "apps" "checks"];
        };
        devShells.default = pkgs.mkShell {
          name = "aoc2023";
          inputsFrom = [config.haskellProjects.default.outputs.devShell];
          nativeBuildInputs = with pkgs; [just];
        };
      };
    };
}
