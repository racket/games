#lang info

(define collection "games")

(define scribblings '(("scribblings/games.scrbl" (multi-page) (gui-library))))

(define gracket-launcher-libraries (list "main.rkt"))
(define gracket-launcher-names (list "PLT Games"))
(define deps '("base"
               "draw-lib"
               "drracket"
               ("gui-lib" #:version "1.16")
               "net-lib"
               "htdp-lib"
               "math-lib"
               "scribble-lib"
               "racket-index"
               "sgl"
               "srfi-lib"
               "string-constants-lib"
               ("data-enumerate-lib" #:version "1.2")
               "typed-racket-lib"
               "typed-racket-more"))
(define build-deps '("draw-doc"
                     "gui-doc"
                     "racket-doc"
                     "pict-lib"
                     "rackunit-lib"
                     "htdp-doc"))

(define pkg-desc "Games")

(define pkg-authors '(mflatt robby))

(define version "1.1")

(define license
  '(Apache-2.0 OR MIT))
