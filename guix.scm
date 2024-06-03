(use-modules
 (guix packages)
 (guix gexp)
 (gnu packages tex)
 (gnu packages texlive)
 (gnu packages guile)
 (gnu packages package-management)
 (guix build-system texlive)
 (guix build-system guile))

(define-public gtex
  (package
    (name "gtex")
    (version "0.1")
    (source
     (local-file "gtex"
                 #:recursive? #t))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-luatex
           texlive-optex))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-script
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (path (string-append out "/bin/gtex")))
                (mkdir (string-append out "/bin"))
                (copy-file
                 #$(local-file "gtex.scm")
                 path)
                (chmod path #o555)))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

gtex
