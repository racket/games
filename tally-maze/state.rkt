#lang racket/base
(require "maze.rkt"
         racket/set
         racket/contract)

(provide
 (struct-out state)
 fill-in-maze
 edge-connecting?
 (contract-out
  [move (-> (or/c 'up 'left 'right 'down) state? state?)]
  [stay-put (-> state? state?)]
  [next-maze (-> state? state?)]
  [game-over? (-> state? boolean?)]
  [initial-state (-> exact-nonnegative-integer?
                     exact-nonnegative-integer?
                     exact-nonnegative-integer?
                     state?)]))

(struct state
  (w h maze-count
     maze-index
     maze edges 
     player
     computer1
     computer2
     player-icon)
  #:transparent)

(define (stay-put this-state)
  (move-computer this-state))

(define (move dir this-state)
  (define-values (dx dy)
    (case dir
      [(left) (values -1 0)]
      [(up) (values 0 -1)]
      [(right) (values 1 0)]
      [(down) (values 0 1)]))
  
  (cond
    [(game-over? this-state) this-state]
    [else
     (define new-x (+ dx (car (state-player this-state))))
     (define new-y (+ dy (cdr (state-player this-state))))
     (define new-pr (cons new-x new-y))
     (cond
       [(and (<= 0 new-x (- (state-w this-state) 1))
             (<= 0 new-y (- (state-h this-state) 1))
             (edge-connecting? (state-edges this-state)
                               (state-player this-state)
                               new-pr))
        (move-computer 
         (struct-copy state this-state [player new-pr]))]
       [else this-state])]))

(define (game-over? this-state)
  (or (computer-won? this-state)
      (player-won? this-state)))

(define (player-won? this-state)
  (and (= (car (state-player this-state)) (- (state-w this-state) 1))
       (= (cdr (state-player this-state)) (- (state-h this-state) 1))))

(define (computer-won? this-state)
  (or (equal? (state-player this-state) 
              (state-computer1 this-state))
      (equal? (state-player this-state) 
              (state-computer2 this-state))))

(define (edge-connecting? edges a b) (set-member? (hash-ref edges a) b))

(define (move-computer the-state)
  (cond
    [(or (equal? (state-player the-state) 
                 (state-computer1 the-state))
         (equal? (state-player the-state) 
                 (state-computer2 the-state)))
     the-state]
    [else
     (define end (state-player the-state))
     (define this-edges (state-edges the-state))
     (define next-edges (state-next-edges the-state))
     
     (define-values (this-maze-c1 this-maze-c1-dist)
       (preferred-direction this-edges (state-computer1 the-state) end))
     (define-values (this-maze-c2 this-maze-c2-dist)
       (preferred-direction this-edges (state-computer2 the-state) end))
     
     (define-values (next-maze-c1 next-maze-c1-dist)
       (preferred-direction next-edges (state-computer1 the-state) end))
     (define-values (next-maze-c2 next-maze-c2-dist)
       (preferred-direction next-edges (state-computer2 the-state) end))
     (cond
       [(<= this-maze-c1-dist this-maze-c2-dist)
        (struct-copy state the-state
                     [computer1 this-maze-c1]
                     [computer2 (if (edge-connecting? this-edges 
                                                      (state-computer2 the-state)
                                                      next-maze-c2)
                                    next-maze-c2
                                    (state-computer2 the-state))])]
       [else
        (struct-copy state the-state
                     [computer1 (if (edge-connecting? this-edges 
                                                      (state-computer1 the-state)
                                                      next-maze-c1)
                                    next-maze-c1
                                    (state-computer1 the-state))]
                     [computer2 this-maze-c2])])]))

(define (state-next-edges this-state)
  (build-walls
   (decode-maze (state-w this-state) (state-h this-state)
                (modulo (+ (state-maze-index this-state) 1)
                        (state-maze-count this-state)))
   (state-w this-state)
   (state-h this-state)))

(define (preferred-direction edges start end)
  (define visited (make-hash))
  (define dir
    (let loop ([node start]
               [dist 0])
      (cond
        [(hash-ref visited node #f) #f]
        [else
         (hash-set! visited node dist)
         (cond
           [(equal? node end) 
            node]
           [else
            (for/or ([neighbor (in-set (hash-ref edges node))])
              (and (loop neighbor (+ dist 1))
                   neighbor))])])))
  (values dir (hash-ref visited end)))

(define (add1/f n) (and n (+ n 1)))


(define (next-maze this-state)
  (define next-maze-state
    (fill-in-maze this-state
                  (modulo (+ (state-maze-index this-state) 1)
                          (state-maze-count this-state))))
  (if (game-over? this-state)
      next-maze-state
      (move-computer next-maze-state)))


(define (fill-in-maze this-state new-val)
  (define current-maze (decode-maze (state-w this-state) (state-h this-state) new-val))
  (struct-copy state this-state
               [maze-index new-val]
               [maze current-maze]
               [edges (build-walls current-maze
                                   (state-w this-state)
                                   (state-h this-state))]))

(define (initial-state maze-w maze-h initial-number)
  (fill-in-maze (state maze-w maze-h (maze-count maze-w maze-h)
                       #f #f #f 
                       (cons 0 0)
                       (cons (- maze-w 1) (- maze-h 1))
                       (cons (- maze-w 1) (- maze-h 1))
                       21)
                initial-number))

(module+ test
  (require math/base rackunit)
  (define moves
    (list (λ (s) (move 'up s))
          (λ (s) (move 'down s))
          (λ (s) (move 'left s))
          (λ (s) (move 'right s))
          stay-put))
  
  ;; returns #t if it isn't possible to win
  ;; using only the moves above
  ;; returns #f it is is possible to win
  ;; does this by searching the space of board
  ;; configurations
  (define (try-to-win size seed)
    (define seen (make-hash))
    
    (let loop ([state (initial-state size size 0)])
      (define key (state->key state))
      (cond
        [(hash-ref seen key #f) #t]
        [(player-won? state) #f]
        [(computer-won? state) #t]
        [else
         (hash-set! seen key #t)
         (for/and ([move (in-list moves)])
           (loop (move state)))])))

  (define (state->key state)
    (list (state-player state)
          (state-computer1 state)
          (state-computer2 state)))

  (check-true
   (let ([size 12])
     (try-to-win size (random-natural (maze-count size size))))))
