#lang racket/base

(require racket/contract
         racket/class
         racket/draw
         "base.rkt"
         "utils.rkt"
         "region.rkt")

;add contracts for region.rkt here to avoid cyclic dependencies

(define make-region/c
  (-> real?
      real?
      (and/c real? (not/c negative?))
      (and/c real? (not/c negative?))
      (or/c string? #f)
      (or/c #f (-> (listof (is-a?/c card<%>)) any))
      any))

(provide table<%>
         card<%>
         make-deck
         make-card
         make-table
         shuffle-list
         struct:region
         region?
         region-button?
         region-hilite?
         region-x
         region-y
         region-w
         region-h 
         region-label
         (contract-out
          [region make-region/c]
          [make-region make-region/c]
          [region-interactive-callback
           (-> region? (or/c #f (-> any/c (listof (is-a?/c card<%>)) any)))]
          [set-region-interactive-callback!
           (-> region? (or/c #f (-> any/c (listof (is-a?/c card<%>)) any))
               any)]
          [region-paint-callback
           (-> region? (or/c #f (-> (is-a?/c dc<%>) real? real? real? real? any)))]
          [set-region-paint-callback!
           (-> region? (or/c #f (-> (is-a?/c dc<%>) real? real? real? real? any))
               any)]
          [region-callback
           (->i ([rgn region?])
                [callback (rgn)
                          (or/c #f (if (region-button? rgn)
                                       (-> any)
                                       (-> (listof (is-a?/c card<%>)) any)))])]
          [set-region-callback!
           (->i ([rgn region?]
                 [callback (rgn)
                           (or/c #f (if (region-button? rgn)
                                        (-> any)
                                        (-> (listof (is-a?/c card<%>)) any)))])
                any)]
          [make-button-region
           (-> real?
               real?
               (and/c real? (not/c negative?))
               (and/c real? (not/c negative?))
               (or/c string? (is-a?/c bitmap%) #f)
               (or/c #f (-> any))
               any)]
          [make-background-region
           (-> real?
               real?
               (and/c real? (not/c negative?))
               (and/c real? (not/c negative?))
               (-> (is-a?/c dc<%>) real? real? real? real? any)
               any)]))
