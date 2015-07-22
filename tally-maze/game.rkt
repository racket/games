#lang racket/base
(require "maze.rkt" 
         "../show-scribbling.rkt"
         "state.rkt"
         racket/gui/base
         racket/class
         racket/set
         racket/list
         racket/path
         racket/runtime-path
         racket/unit)

(provide game@)

(define-runtime-path bmps "images")
(define big-pumpkin (read-bitmap (build-path bmps "pumpkin" "pumpkin-64x64.png")))
(define two-pumpkins (make-bitmap (send big-pumpkin get-width) (send big-pumpkin get-height)))
(let ([small-pumpkin (read-bitmap (build-path bmps "pumpkin" "pumpkin-48x48.png"))]
      [bdc (make-object bitmap-dc% two-pumpkins)])
  (send bdc draw-bitmap small-pumpkin 0 0)
  (send bdc draw-bitmap small-pumpkin
        (- (send big-pumpkin get-width)
           (send small-pumpkin get-width))
        (- (send big-pumpkin get-height)
           (send small-pumpkin get-height)))
  (send bdc set-bitmap #f))

(define small-icon-size 4)
(define (mk-small color)
  (define bmp (make-bitmap small-icon-size small-icon-size))
  (define bdc (make-object bitmap-dc% bmp))
  (send bdc set-brush color 'solid)
  (send bdc set-pen "black" 1 'transparent)
  (send bdc draw-ellipse 0 0 small-icon-size small-icon-size)
  (send bdc set-bitmap #f)
  bmp)

(define small-pumpkin (mk-small "orange"))
(define small-player (mk-small "blue"))

(define game@
  (unit (import)
        (export)


(define the-states
  (let ([game-size 12])
    (list (initial-state game-size game-size (pick-a-maze game-size game-size)))))
(define (current-state) (car the-states))
(define (set-the-states! new-states)
  (set! the-states new-states)
  (send game-number-canvas refresh)
  (send game-canvas refresh))

(define (next-state! state)
  (set-the-states! (cons state the-states)))

(define (get-player-icon the-state)
  (cond
    [(and (= (car (state-player the-state)) (- (state-w the-state) 1))
          (= (cdr (state-player the-state)) (- (state-h the-state) 1)))
     ;; winner
     (pick '(1))]
    [(edge-connecting? (state-edges the-state)
                       (state-player the-state)
                       (cons (- (state-w the-state) 1)
                             (- (state-h the-state) 1)))
     ;; about to win
     (pick '(19))]
    [(or (edge-connecting? (state-edges the-state)
                           (state-computer1 the-state)
                           (state-player the-state))
         (edge-connecting? (state-edges the-state)
                           (state-computer2 the-state)
                           (state-player the-state)))
     ;; about to lose
     (pick '(20 35))]
    [else 
     ;; nothing much going on
     (pick '(21 36 37))]))

(define (pick args)
  (define pr (state-player (current-state)))
  (list-ref args (modulo (+ (car pr) (cdr pr))
                         (length args))))

(define players (make-hash))
(for ([file (directory-list (build-path bmps "very-emotional") #:build? #t)])
  (define m (regexp-match #rx"([0-9]+)[.]png$" file))
  (when m
    (hash-set! players (string->number (cadr m)) (read-bitmap file))))

(define (move! dir) (next-state! (move dir (current-state))))
(define (stay-put!) (next-state! (stay-put (current-state))))
(define (next-maze!) (next-state! (next-maze (current-state))))
(define (undo-maze!)
  (unless (null? (cdr the-states))
    (set-the-states! (cdr the-states))))

(define game-number-canvas%
  (class canvas%
    (inherit get-dc get-client-size)
    (define/override (on-paint)
      (define-values (w h) (get-client-size))
      (define dc (get-dc))
      (define s (str (state-maze-index (current-state))))
      (define-values (tw _1 _2 _3) (send dc get-text-extent s))
      (send dc draw-text s (/ (- w tw) 2) 0))
    (define/private (str n) (format "Board #~a" n))
    (super-new [style '(transparent)])
    (inherit min-height)
    (let ()
      (define-values (_ h _2 _3) (send (get-dc) get-text-extent (str 0)))
      (min-height (inexact->exact (ceiling h))))))

(define game-canvas%
  (class canvas%
    (inherit get-dc get-client-size)
    (define/override (on-paint)
      (define dc (get-dc))
      (send dc set-smoothing 'smoothed)
      (define-values (w h) (get-client-size))
      (draw-a-state dc 0 0 w h (current-state) #f))
    (define/override (on-char evt) 
      (case (send evt get-key-code)
        [(left) (move! 'left)]
        [(up) (move! 'up)]
        [(right) (move! 'right)]
        [(down) (move! 'down)]
        [(#\space #\.) (stay-put!)]
        [(#\n) (next-maze!)]
        [(#\z) (undo-maze!)]))
    (super-new)))

(define (draw-a-state dc dx dy w h the-state small?)
  (draw-maze dc dx dy 
             w h (state-edges the-state)
             (state-w the-state)
             (state-h the-state)
             #:images 
             (cons (list (if small?
                             (list small-player) 
                             (list (hash-ref players (get-player-icon the-state))))
                         (car (state-player the-state))
                         (cdr (state-player the-state)))
                   (if (equal? (state-computer1 the-state)
                               (state-computer2 the-state))
                       (list (list (if small? (list small-pumpkin) (list two-pumpkins))
                                   (car (state-computer1 the-state))
                                   (cdr (state-computer1 the-state))))
                       (list (list (if small? (list small-pumpkin) (list big-pumpkin))
                                   (car (state-computer1 the-state))
                                   (cdr (state-computer1 the-state)))
                             (list (if small? (list small-pumpkin) (list big-pumpkin))
                                   (car (state-computer2 the-state))
                                   (cdr (state-computer2 the-state))))))))

(define min-cell-size 55)
(define f (new frame% [label "Tally Maze"] [width 600] [height 600]))
(define game-canvas (new game-canvas% 
                         [parent f]
                         [min-width (* (state-w (current-state)) min-cell-size)]
                         [min-height (* (state-h (current-state)) min-cell-size)]))
(define hp (new horizontal-panel% [parent f] [alignment '(right center)] [stretchable-height #f]))
(define game-number-canvas (new game-number-canvas% [parent hp] [stretchable-height #f]))
(void (new vertical-panel% [parent hp] [stretchable-width #f]))
(define show-help (show-scribbling
                   '(lib "games/scribblings/games.scrbl")
                   "tally-maze"))
(define help-button (new button%
                         [label "Help"]
                         [parent hp]
                         [callback (lambda (_1 _2) (show-help))]))
(send f show #t)))

(module+ main (invoke-unit game@))
