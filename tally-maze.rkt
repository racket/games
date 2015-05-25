#lang racket/base
(require (submod "tally-maze/game.rkt" main))
(module test racket/base) ;; avoid launching GUI in tests
