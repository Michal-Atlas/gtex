{...}: {
  perSystem = {
    self',
    pkgs,
    ...
  }:
    with self'.flakeTex;
    with builtins; {
      apps = {};
      packages.example = mkDocument {
        name = "Example presentation with Flake\\TeX{}";
        slides = true;
        chapters = [
          (svgImage {
            name = "What is a sequent?";
            src = fetchurl {
              url = "https://wikimedia.org/api/rest_v1/media/math/render/svg/e0c5b66a8fed889a35d2ea5ff95f0c604351346d";
              sha256 = "sha256:1q29lym3sfxpvr4kp4xkdq69bl248i32rg0cgh5f5nkhxq5bs86z";
            };
          })
          (image {
            name = "Sunset";
            src = fetchurl {
              name = "sunset";
              url = "https://external-content.duckduckgo.com/iu/?u=https%3A%2F%2Fimages.pexels.com%2Fphotos%2F417211%2Fpexels-photo-417211.jpeg%3Fcs%3Dsrgb%26dl%3Dafterglow-beach-clouds-417211.jpg%26fm%3Djpg&f=1&nofb=1&ipt=acb1933c0b4fbaa54acf4493f90d21ce113bed33484cbf966557510cbb74be0f&ipo=images";
              sha256 = "sha256:07xzb4698k418jrgmb0ah9mgh3ssy0qc9wqhj5k0xbfl0iljzpfd";
            };
          })
          (title "What was that?")
          (codeLines {
            name = "Python 3";
            runner = "${pkgs.python3}/bin/python";
            src = [
              "print(3)"
              "print 3"
              "2 + 3"
            ];
          })
          (codeLines {
            name = "Python 2";
            runner = "${pkgs.python2}/bin/python";
            src = [
              "print(3)"
              "print 3"
              "2 + 3"
            ];
          })
          (code {
            name = "Larger program";
            runner = "${pkgs.sbcl}/bin/sbcl --script";
            src = ''
              (defun myfunc ()
                (loop for i to 4
                  do (format t "Hello ~a~%" i)))

              (myfunc)
            '';
          })
          (bigtt "What is love?" ''
            Baby don't hurt me
          '')
        ];
      };
    };
}
