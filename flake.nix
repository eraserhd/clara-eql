{
  description = "clara-eql: Generate Clara rules to collect data from EDN Query Language queries.";
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };
  outputs = { self, nixpkgs, flake-utils }:
    (flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # `deps.nix` is generated with:
        #
        #     $ nix run github:hlolli/clj2nix -- deps.edn deps.nix -A:test -A:cljs-test
        #
        cljdeps = import ./deps.nix {
          inherit (pkgs) fetchMavenArtifact fetchgit lib;
        };
        classpath = cljdeps.makeClasspaths {
          extraClasspaths = [ "./src" "./test" ];
        };
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            clojure
            nodejs_24
          ];
        };
        checks = {
          tests = pkgs.stdenv.mkDerivation {
            name = "clara-eql-tests";
            src = ./.;
            buildInputs = with pkgs; [
              clojure
              git
              nodejs_24
            ];
            buildPhase = ''
              set -x
              mkdir tmp
              HOME=tmp clojure -Scp ${classpath} -M:test
              HOME=tmp clojure -Scp ${classpath} -M:cljs-test
            '';
            installPhase = "touch $out";
          };
        };
    }));
}
