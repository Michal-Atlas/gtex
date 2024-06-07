#!/usr/bin/env -S guix repl
!#
(define-module (gtex)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages)
  #:use-module (ice-9 textual-ports)
  #:use-module (guix store)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix profiles)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:export (build-inputs
            eval-script))

(define (string-split-first str c)
  (let* ((spl (string-index str c)))
    (if spl
        (values (string-take str spl)
                (string-drop str (1+ spl)))
        (values str ""))))

(define (build-inputs file)
  (let ((inputs
         (map
          (lambda (line)
            (let-values (((id cmd) (string-split-first line #\:)))
              (cons id (eval-string cmd))))
          (string-split
           (string-trim-both
            (call-with-input-file file get-string-all))
           #\newline))))
    (with-store
     %store
     (let ((inp-file
            (string-append
             (string-drop-right file 3)
             "inp"))
           (mfile
            (run-with-store
             %store
             (lower-object
              (computed-file
               "document-inputs"
               #~(with-output-to-file #$output
                   (lambda ()
                     ((@ (ice-9 format) format) #t "~{~{~a:~a~}~%~}"
                      (map list
                           '#$(map car inputs)
                           '#$(map cdr inputs))))))))))
       (build-derivations %store (list mfile))
       (when (file-exists? inp-file)
         (delete-file inp-file))
       (copy-file
        (derivation->output-path mfile)
        inp-file)))))

(define (packages->profile pkgs)
  (with-store %store
    (run-with-store %store
      (profile-derivation (packages->manifest pkgs)))))

(define (bash-script cmd)
  (if (string-contains cmd "|")
      (let*-values (((packages cmd) (string-split-first cmd #\|))
                    ((first-cmd rest-cmd) (string-split-first (string-trim-both cmd) #\space)))
        (let ((packages (map (compose specification->package
                                      string-trim-both)
                             (string-split (string-trim-both packages) #\,)))
              (cmd (string-append first-cmd " \"$@\" " rest-cmd)))
              
          (mixed-text-file
           "script.sh"
           "GUIX_PROFILE=\"" (packages->profile packages) "\"\n"
           ". \"$GUIX_PROFILE/etc/profile\"\n"
           cmd)))

      (let*-values (((runner args) (string-split-first cmd #\space))
                    ((pkg file) (string-split-first runner #\/)))
        (let ((runner
               (file-append (specification->package pkg)
                            (string-append "/bin/"
                                           (if (zero? (string-length file))
                                               pkg file)
                                           " " args))))
          (mixed-text-file "raw-script.sh"
                           runner " \"$@\"\n")))))

(define* (eval-script spec script
                      #:optional
                      (filename "script-result")
                      out)
  (computed-file
   filename
   #~(call-with-output-file #$output
       (lambda (out)
         (waitpid
          (spawn #$(file-append bash "/bin/bash")
                 (list "bash" #$(bash-script spec) #$script)
                 #:search-path? #f
                 #:output out
                 #$@(if out '(#:error out) '())))))))


(define (read-gtex-string chr port)
  (with-output-to-string
    (lambda ()
      (let loop ((c (read-char port))
                 (ignore #f))
        (let ((re (lambda (i) (loop (read-char port) i))))
          (unless (and (not ignore) (eq? c #\»))
            (if ignore
                (case c
                  ((#\») (display c) (re #f))
                  ((#\n) (display #\newline) (re #f))
                  (else (display #\\) (display c) (re #f)))
                (if (eq? c #\\)
                    (re #t)
                    (begin (display c) (re #f))))))))))

(read-hash-extend #\« read-gtex-string)

(if (= 2 (length (command-line)))
    (build-inputs (cadr (command-line)))
    (display "Usage: gtex <aux file>"))
