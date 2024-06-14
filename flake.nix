{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };

  outputs = {
    nixpkgs,
    flake-parts,
    systems,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;}
    {
      systems = import systems;
      perSystem = {
        self',
        pkgs,
        system,
        ...
      }: {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [
            (pkgs.texliveSmall.withPackages (tp:
              with tp; [
                self'.packages.gtex
                fira
                firamath
                xits
              ]))
          ];
        };
        packages = {
          gtex = pkgs.stdenvNoCC.mkDerivation {
            pname = "optex-gtex";
            version = "trunk";
            passthru.tlDeps = with pkgs.texlive; [optex];
            outputs = ["out" "tex"];
            src = ./src;
            dontPatchShebangs = true;
            installPhase = ''
              dir=$tex/tex/luatex/gtex
              mkdir -p $dir $out/bin
              cp -r $src/* $dir
              mv $dir/gtex.scm $out/bin/gtex
            '';
          };
        };
      };
    };
}
