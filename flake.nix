{
  description = "TODO: fill me in";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        clara-eql = pkgs.callPackage ./derivation.nix {};
      in {
        packages = {
          default = clara-eql;
          inherit clara-eql;
        };
        checks = {
          test = pkgs.runCommandNoCC "clara-eql-test" {} ''
            mkdir -p $out
            : ${clara-eql}
          '';
        };
    })) // {
      overlays.default = final: prev: {
        clara-eql = prev.callPackage ./derivation.nix {};
      };
    };
}
