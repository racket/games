#lang racket/base

(require racket/class
         racket/contract
         "make-cards.rkt"
         "classes.rkt"
         "card-class.rkt")

(provide make-card
         table<%>
         card<%>
         make-deck
         (contract-out
          [make-table
           (->* {}
                {string?
                 real?
                 real?
                 #:mixin (make-mixin-contract table<%>)}
                any)]
          ))

(define table<%> (class->interface table%))
(define card<%> (class->interface card%))

(define (make-table [title "Cards"]
                    [w 7]
                    [h 3]
                    #:mixin [mixin values])
  (make-object (mixin table%) title w h))

(define (make-deck)
  (map (lambda (l) (send l copy)) deck-of-cards))
