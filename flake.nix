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
            outputs = ["out" "tex" "texdoc"];
            src = ./src;
            dontPatchShebangs = true;
            nativeBuildInputs = [(pkgs.texliveMinimal.withPackages (pt: [pt.optex]))];
            buildPhase = ''
              export TEXMFCACHE=$PWD
              cp $src/* .
              function tex () {
                optex --jobname=gtex '\docgen gtex';
              }
              while tex |& grep "TeX me again"
              do :
              done;
            '';
            installPhase = ''
              texdir=$tex/tex/luatex/gtex
              docdir=$texdoc/doc/gtex
              mkdir -p $texdir $out/bin $docdir
              cp -r $src/* $texdir
              mv $texdir/gtex.scm $out/bin/gtex
              mv *.pdf $docdir
            '';
          };
        };
      };
    };
}
