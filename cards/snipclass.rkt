#lang racket/base

(require racket/gui/base
         racket/class)

(provide sc)

(define sc (make-object snip-class%))
(send sc set-classname "card")
(send (get-the-snip-class-list) add sc)

