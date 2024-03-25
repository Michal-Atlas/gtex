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
      perSystem = {
        pkgs,
        system,
        lib,
        ...
      }: {
        packages = rec {
          default = document;
          document = let
            codeExample = {
              name,
              runner,
              src,
              ...
            }: rec {
              inherit name;
              runnerOutput =
                builtins.readFile
                (pkgs.runCommand "${name}-slide-builder.sh" {} ''
                  cat ${pkgs.writeText "${name}-runner-inputs.src" src} | ${runner} > $out
                '');
              body = ''
                \begtt \hisyntax{python}
                ${src}
                \endtt

                \begtt
                ${runnerOutput}
                \endtt
              '';
            };
            lowerChapter = document: {
              name,
              body,
              ...
            }: ''
              \sec ${name}

              ${body}
              ${
                if document.slides
                then "\\pg;"
                else ""
              }
            '';
            mkDocument = {
              name,
              chapters ? [],
              packages ? [],
              slides ? false,
              ...
            } @ inputs:
              pkgs.stdenv.mkDerivation {
                inherit name;
                src = pkgs.writeText "document.tex" ''
                  ${
                    if slides
                    then ''
                      \slides
                      \wideformat
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
            luaInsertARPreserveImage = pkgs.writeText "ap-image.lua" ''
              function insertAPImage(image)
                local i = img.scan { filename = image };
                local ratio = 0;
                    if (i.width > i.height) then
                      ratio = i.width / tex.vsize;
                    else
                      ratio = i.height / tex.hsize;
                    end
                    i.width = i.width / ratio;
                    i.height = i.height / ratio;
                    img.write(i);
              end
            '';
          in
            mkDocument rec {
              name = "Greetings";
              slides = true;
              packages = [];
              chapters = [
                {
                  name = "Jep tjer";
                  body = ''
                    {\bf bold} one.
                    * A
                    * H
                    \begitems
                    * B
                    * L
                    \enditems
                    * C
                  '';
                }
                {
                  name = "Image";
                  body = ''
                    %\noindent \picw=\hsize \picheight=0.8\vsize \inspic
                    \backgroundpic{${builtins.fetchurl {
                      url = "https://www.photos-public-domain.com/wp-content/uploads/2011/11/brilliant-sunrise-over-houses.jpg";
                      sha256 = "sha256:1ay8ihwkfmvjx998883a352rs3inkdwhhv67hld44lnjkymwynr5";
                    }}}
                  '';
                }
                {
                  name = "AR MF";
                  body = ''
                    \directlua{dofile('${luaInsertARPreserveImage}')}
                    \directlua{ insertAPImage("${builtins.fetchurl {
                      url = "https://www.photos-public-domain.com/wp-content/uploads/2011/11/brilliant-sunrise-over-houses.jpg";
                      sha256 = "sha256:1ay8ihwkfmvjx998883a352rs3inkdwhhv67hld44lnjkymwynr5";
                    }}") }
                  '';
                }
                (codeExample {
                  name = "Python";
                  runner = "${pkgs.python3}/bin/python";
                  src = ''
                    for i in range(5):
                        print(i)
                  '';
                })
                (codeExample {
                  name = "SBCL";
                  runner = "${pkgs.sbcl}/bin/sbcl --script";
                  src = ''
                    (print "Hello")
                    (princ "Hello")
                  '';
                })
              ];
            };
        };
      };
    };
}
