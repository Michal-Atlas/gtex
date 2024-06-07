(use-modules
 (guix packages)
 (guix gexp)
 (gnu packages tex)
 (guix build-system copy))

(define-public gtex
  (package
    (name "gtex")
    (version "0.1")
    (source
     (local-file "src"
                 #:recursive? #t))
    (build-system copy-build-system)
    (propagated-inputs (list texlive-optex))
    (arguments
     (list #:install-plan
            #~`(("." "share/texmf-dist/tex/luatex/gtex/" #:exclude ("gtex.scm"))
                ("gtex.scm" "bin/gtex"))))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

gtex
