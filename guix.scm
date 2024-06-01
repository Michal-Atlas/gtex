(use-modules
 (guix packages)
 (guix gexp)
 (gnu packages tex)
 (gnu packages texlive)
 (gnu packages guile)
 (gnu packages package-management)
 (guix build-system texlive)
 (guix build-system guile))

(define guile-gtex
  (package
    (name "guile-gtex")
    (version "0.1")
    (source (local-file "gtex.scm"))
    (build-system guile-build-system)
    (propagated-inputs (list guile-3.0 guix))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public gtex
  (package
    (name "gtex")
    (version "0.1")
    (source (local-file "gtex"
                        #:recursive? #t))
    (build-system texlive-build-system)
    (propagated-inputs (list guile-gtex))
    (native-inputs
     (list texlive-luatex
           texlive-optex))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-script
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir (string-append out "/bin"))
                (copy-file
                 #$(program-file
                    "gtex"
                    #~(begin
                        (use-modules (gtex))
                        (if (= 2 (length (command-line)))
                            (build-inputs (cadr (command-line)))
                            (display "Usage: gtex <aux file>"))))
                 (string-append out "/bin/gtex"))))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

gtex
