#lang racket/base

;add contracts for region.rkt here to avoid cyclic dependencies
(module region/contracted racket/base
  (require racket/contract
           racket/class
           racket/draw
           "base.rkt"
           "region.rkt")
  (provide (contract-out
            [struct region ([x real?]
                            [y real?]
                            [w (and/c real? (not/c negative?))]
                            [h (and/c real? (not/c negative?))]
                            [label (or/c string? #f)]
                            [callback (or/c #f (-> (listof (is-a?/c card<%>)) any))]
                            [interactive-callback
                             (or/c #f (-> any/c (listof (is-a?/c card<%>)) any))]
                            [paint-callback
                             (or/c #f (-> (is-a?/c dc<%>) real? real? real? real?
                                          any))]
                            [button? any/c]
                            [hilite? any/c]
                            [decided-start? any/c] 
                            [can-select? any/c])] 
            [make-button-region
             (-> real?
                 real?
                 (and/c real? (not/c negative?))
                 (and/c real? (not/c negative?))
                 (or/c string? #f)
                 (or/c #f (-> (listof (is-a?/c card<%>)) any))
                 any)]
            [make-background-region
             (-> real?
                 real?
                 (and/c real? (not/c negative?))
                 (and/c real? (not/c negative?))
                 (-> (is-a?/c dc<%>) real? real? real? real? any)
                 any)]
            )))

(require racket/contract
         racket/class
         racket/draw
         "base.rkt"
         "utils.rkt"
         (submod "." region/contracted))

(provide table<%>
         card<%>
         make-deck
         make-card
         make-table
         shuffle-list
         region
         struct:region
         make-region
         region?
         region-x
         region-y
         region-w
         region-h 
         region-label
         region-callback set-region-callback!
         region-interactive-callback set-region-interactive-callback!
         region-paint-callback set-region-paint-callback!
         region-button?
         region-hilite?
         make-button-region
         make-background-region
         )