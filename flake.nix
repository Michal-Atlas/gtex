{
  inputs = {
    nixpkgs.url = "github:NixOs/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    systems.url = "github:nix-systems/default";
  };
  outputs = {
    flake-parts,
    systems,
    ...
  } @ inputs:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = import systems;
      transposition.flakeTex.adHoc = true;
      perSystem = {
        self',
        pkgs,
        lib,
        ...
      }: {
        flakeTex = rec {
          runnerCat = {
            name,
            runner,
            onlyLast ? false,
          }: src:
            builtins.readFile
            (pkgs.runCommand "${name}-slide-builder.sh" {} ''
              cat ${pkgs.writeText "${name}-runner-inputs.src" src} | ${runner} > $out
            '');
          code = {
            name,
            runner,
            src,
            ...
          }: rec {
            inherit name;
            runnerOutput = runnerCat {inherit name runner;};
            body = ''
              \begtt \hisyntax{python}
              ${src}
              \endtt

              \begtt
              ${runnerOutput}
              \endtt
            '';
          };
          codeLines = {
            name,
            runner,
            src,
            printer ? x: x,
            ...
          }: rec {
            inherit name;
            runnerOutputs =
              builtins.map (runnerCat {
                inherit name runner;
                onlyLast = true;
              })
              (builtins.map printer src);
            body = ''
              \begtt \hisyntax{python}
              ${builtins.concatStringsSep "\n" (builtins.map (
                {
                  fst,
                  snd,
                }: "${fst} => ${snd}"
              ) (lib.lists.zipLists src runnerOutputs))}
              \endtt
            '';
          };
          svgImage = {src, ...} @ inputs:
            image (inputs
              // {
                src = "${pkgs.runCommand "inkscape-creator" {inherit src;} ''
                  mkdir -p $out
                  ${pkgs.inkscape}/bin/inkscape $src -o $out/img.png -w 1920
                ''}/img.png";
              });
          image = {
            name,
            src,
            ...
          }: {
            inherit name;
            body = ''
              \hbox{\hfil\directlua{insertAPImage("${src}")}\hfil}
            '';
          };
          lowerChapter = document: {
            name,
            body ? "",
            ...
          }: ''
            {
              \sec ${name}
              ${
              if document.slides
              then "\\vfil"
              else ""
            }
              ${body}
              ${
              if document.slides
              then "\\vfil"
              else ""
            }
            }
              ${
              if document.slides
              then "\\pg;"
              else ""
            }
          '';
          luaInsertARPreserveImage = pkgs.writeText "ap-image.lua" ''
            function insertAPImage(image)
            local i = img.scan { filename = image };
            local ratio = 0;
            if (i.width > i.height) then
            ratio = i.width / tex.hsize;
            else
            ratio = i.height / tex.vsize;
            end
            i.width = i.width / ratio;
            i.height = i.height / ratio;
            img.write(i);
            end
          '';
          title = title: {
            name = "";
            body = "\\tit{${title}}";
          };
          bigtt = name: body: {
            inherit name;
            body = ''
              \everytt={\typosize[24/27]}
              \begtt
              ${body}
              \endtt
            '';
          };
          mkDocument = {
            name,
            prelude ? "",
            chapters ? [],
            packages ? [],
            slides ? false,
            ...
          } @ inputs:
            pkgs.stdenv.mkDerivation {
              inherit name;
              src = pkgs.writeText "document.tex" ''
                \fontfam[Fira]
                ${prelude}
                \directlua{dofile('${luaInsertARPreserveImage}')}
                ${
                  if slides
                  then ''
                    \slides
                    \wideformat
                    \colordef \ColourBg {0.98\Red+0.98\Blue+0.98\Green}
                    \colordef \ColourTxt {0.14\Red+0.21\Blue+0.23\Green}
                    \_let\_scolor=\ColourTxt
                    \pgbackground={\ColourBg \hrule height 0pt depth\pdfpageheight width\pdfpagewidth}
                    \slideshow
                  ''
                  else ""
                }
                %\load[${builtins.concatStringsSep ", " packages}]
                \tit ${name}
                ${
                  if slides
                  then "\\pg;\n"
                  else ""
                }
                ${builtins.concatStringsSep "\n" (builtins.map (lowerChapter inputs) chapters)}
                \bye
              '';
              buildInputs = [
                (pkgs.texlive.combine ({
                    inherit
                      (pkgs.texlive)
                      scheme-basic
                      optex
                      fira
                      firamath
                      xits
                      ;
                  }
                  // (builtins.listToAttrs (builtins.map (name: {
                      inherit name;
                      value = lib.attrsets.getAttrFromPath [name] pkgs.texlive;
                    })
                    packages))))
              ];
              dontUnpack = true;
              buildPhase = ''
                export TEXMFCACHE=$PWD
                optex $src || cat *.log
              '';
              installPhase = ''
                mv *.pdf $out
              '';
            };
        };
      };
    };
}
