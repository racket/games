#lang racket/base

;no contracts here because the callback contract uses card<%>

(provide (struct-out region)
         make-button-region
         make-background-region)

(struct region (x
                y
                w
                h
                label
                [callback #:mutable]
                [interactive-callback #:auto #:mutable]
                [paint-callback #:auto #:mutable]
                [button? #:auto #:mutable]
                [hilite? #:auto #:mutable]
                [decided-start? #:auto #:mutable]
                [can-select? #:auto #:mutable])
  #:extra-constructor-name make-region)

(define (make-background-region x y w h paint-callback)
  (let ([r (make-region x y w h #f #f)])
    (set-region-paint-callback! r paint-callback)
    r))

(define (make-button-region x y w h label callback)
  (let ([r (make-region x y w h label callback)])
    (set-region-button?! r #t)
    r))
