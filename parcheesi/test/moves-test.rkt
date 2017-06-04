#lang racket/base

(require "../moves.rkt"
         "../board.rkt"
         "../moves.rkt"
         "test.rkt"
         racket/vector)
    
;; mb : move ... -> board
;; builds a board by applying `moves' to a new board
(define (mb . moves)
  (let-values ([(board bonus) (make-moves (new-board) moves)])
    board))
  
(define blue0 (make-pawn 'blue 0))
(define blue1 (make-pawn 'blue 1))
(define blue2 (make-pawn 'blue 2))
(define blue3 (make-pawn 'blue 3))
(define green0 (make-pawn 'green 0))
(define green1 (make-pawn 'green 1))
(define green2 (make-pawn 'green 2))
(define green3 (make-pawn 'green 3))
(define yellow0 (make-pawn 'yellow 0))
(define yellow1 (make-pawn 'yellow 1))
(define yellow2 (make-pawn 'yellow 2))
(define yellow3 (make-pawn 'yellow 3))
(define red0 (make-pawn 'red 0))
(define red1 (make-pawn 'red 1))
(define red2 (make-pawn 'red 2))
(define red3 (make-pawn 'red 3))
  
(test (<=/m 0 1 2) #t)
(test (<=/m 0 2 1) #f)
(test (<=/m 1 0 2) #f)
  
(test (<=/m 4 0 1) #t)
(test (<=/m 4 1 0) #f)
(test (<=/m 0 4 1) #f)
  
(test (<=/m 3 4 1) #t)
(test (<=/m 3 1 4) #f)
(test (<=/m 4 3 1) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; blockade finding tests
;;
  
(let ([board (make-board (list blue0 blue1 blue2 blue3
                               green2 green3 
                               red0 red1 red2 red3)
                         `#68(() () (,yellow1 ,yellow2) ())
                         (list (cons 'blue `#7((,blue0 ,blue1) ()))
                               (cons 'green `#7(() () () (,green0 ,green1) ()))
                               (cons 'red (make-vector 7 '()))
                               (cons 'yellow (make-vector 7 '())))
                         (list yellow0 yellow3))])
  (test (find-blockade/between board 0 1) #f)
  (test (find-blockade/between board 0 2) 2)
  (test (find-blockade/between board 0 5) 2)
  (test (find-blockade/between board 60 1) #f)
  (test (find-blockade/between board 60 2) 2)
  (test (find-blockade/between board 60 5) 2)
  (test (find-blockade/between board 
                               (make-home-row-loc 0 'red)
                               (make-home-row-loc 7 'red))
        #f)
  (test (find-blockade/between board 
                               (make-home-row-loc 0 'blue)
                               (make-home-row-loc 7 'blue))
        (make-home-row-loc 0 'blue))
  (test (find-blockade/between board 
                               (make-home-row-loc 0 'green)
                               (make-home-row-loc 7 'green))
        (make-home-row-loc 3 'green))
  (test (find-blockade/between board 
                               66
                               (make-home-row-loc 2 'green))
        #f)
  (test (find-blockade/between board 
                               66
                               (make-home-row-loc 5 'green))
        (make-home-row-loc 3 'green))
  (test (find-blockade/between board 
                               1
                               (make-home-row-loc 2 'green))
        2))
  
(test (find-blockades/color (new-board) 'green)
      '())
  
(test (find-blockades/color (make-board '()
                                        `#68(())
                                        (list (cons 'blue `#7(()))
                                              (cons 'green `#7(()))
                                              (cons 'red (make-vector 7 '()))
                                              (cons 'yellow (make-vector 7 '())))
                                        (list blue0 blue1 blue2 blue3
                                              green2 green3 
                                              red0 red1 red2 red3
                                              yellow0 yellow1 yellow2 yellow3))
                            'yellow)
      '())
  
(test (find-blockades/color (make-board '()
                                        `#68((,green0 ,green1) ())
                                        (list (cons 'blue `#7(()))
                                              (cons 'green `#7(()))
                                              (cons 'red (make-vector 7 '()))
                                              (cons 'yellow (make-vector 7 '())))
                                        (list blue0 blue1 blue2 blue3
                                              green2 green3 
                                              red0 red1 red2 red3
                                              yellow0 yellow1 yellow2 yellow3))
                            'yellow)
      '())
  
(test (find-blockades/color (make-board '()
                                        `#68((,green0 ,green1) ())
                                        (list (cons 'blue `#7(()))
                                              (cons 'green `#7(()))
                                              (cons 'red (make-vector 7 '()))
                                              (cons 'yellow (make-vector 7 '())))
                                        (list blue0 blue1 blue2 blue3
                                              green2 green3 
                                              red0 red1 red2 red3
                                              yellow0 yellow1 yellow2 yellow3))
                            'green)
      (list (make-blockade 0 (make-pawn 'green 0) (make-pawn 'green 1))))
  
(test (find-blockades/color (make-board '()
                                        `#68((,green0 ,green1) (,green2 ,green3) ())
                                        (list (cons 'blue `#7(()))
                                              (cons 'green `#7(()))
                                              (cons 'red (make-vector 7 '()))
                                              (cons 'yellow (make-vector 7 '())))
                                        (list blue0 blue1 blue2 blue3
                                              red0 red1 red2 red3
                                              yellow0 yellow1 yellow2 yellow3))
                            'green)
      (list (make-blockade 1 (make-pawn 'green 2) (make-pawn 'green 3))
            (make-blockade 0 (make-pawn 'green 0) (make-pawn 'green 1))))
  
(test (find-blockades/color (make-board '()
                                        `#68(() (,green2 ,green3) ())
                                        (list (cons 'blue `#7(()))
                                              (cons 'green `#7(() (,green0 ,green1) ()))
                                              (cons 'red (make-vector 7 '()))
                                              (cons 'yellow (make-vector 7 '())))
                                        (list blue0 blue1 blue2 blue3
                                              red0 red1 red2 red3
                                              yellow0 yellow1 yellow2 yellow3))
                            'green)
      (list (make-blockade 1
                           (make-pawn 'green 2) 
                           (make-pawn 'green 3))
            (make-blockade (make-home-row-loc 1 'green) 
                           (make-pawn 'green 0)
                           (make-pawn 'green 1))))
  
(test (find-blockades/color
       (make-board '()
                   `#68(())
                   (list (cons 'blue `#7(()))
                         (cons 'green `#7((,green2 ,green3) (,green0 ,green1) ()))
                         (cons 'red (make-vector 7 '()))
                         (cons 'yellow (make-vector 7 '())))
                   (list blue0 blue1 blue2 blue3
                         red0 red1 red2 red3
                         yellow0 yellow1 yellow2 yellow3))
       'green)
      (list (make-blockade (make-home-row-loc 0 'green) 
                           (make-pawn 'green 2) 
                           (make-pawn 'green 3))
            (make-blockade (make-home-row-loc 1 'green) 
                           (make-pawn 'green 0) 
                           (make-pawn 'green 1))))
  
(test (find-blockades/color
       (make-board '()
                   `#68(())
                   (list (cons 'blue `#7(()))
                         (cons 'green `#7((,green2 ,green0) (,green3 ,green1) ()))
                         (cons 'red (make-vector 7 '()))
                         (cons 'yellow (make-vector 7 '())))
                   (list blue0 blue1 blue2 blue3
                         red0 red1 red2 red3
                         yellow0 yellow1 yellow2 yellow3))
       'green)
      (list (make-blockade (make-home-row-loc 1 'green) 
                           (make-pawn 'green 1) 
                           (make-pawn 'green 3))
            (make-blockade (make-home-row-loc 0 'green)
                           (make-pawn 'green 0) 
                           (make-pawn 'green 2))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; moving tests
;;

;; enter
(let ([board (new-board)])
  (test (board-enter-piece board green0)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () (,green0) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; enter with bop
(let*-values ([(board1) (new-board)]
              [(board2 _1) (board-enter-piece board1 green0)]
              [(board3 _2) (board-move-piece-main board2 green0 5 17)])
  (test (board-enter-piece board3 red0)
        (make-board (list blue0 blue1 blue2 blue3
                          green0 green1 green2 green3 
                          red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () () () ()
                            ()
                            () () () () () () () ()
                            () () () () () (,red0) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        20))
  
  
;; move with bop
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 red0)]
              [(board4 bonus4) (board-move-piece-main board3 red0 22 1)])
  (test (board-move-piece-main board4 green0 5 18)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () () () ()
                            ()
                            () () () () () () () ()
                            () () () () () () (,green0) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        20))
  
;; move on a bop -- should not get a new bop bonus
(test (board-move-piece-main (make-board (list blue0 blue1 blue2 blue3
                                               green1 green2 green3 
                                               red1 red2 red3 
                                               yellow0 yellow1 yellow2 yellow3)
                                         (let ([v (make-vector 68 '())])
                                           (vector-set! v 21 (list green0))
                                           (vector-set! v 25 (list red0))
                                           v)
                                         (list (cons 'blue (make-vector 7 '()))
                                               (cons 'green (make-vector 7 '()))
                                               (cons 'red (make-vector 7 '()))
                                               (cons 'yellow (make-vector 7 '())))
                                         '())
                             green0
                             21
                             4)
      (make-board (list blue0 blue1 blue2 blue3
                        green1 green2 green3 
                        red0 red1 red2 red3 
                        yellow0 yellow1 yellow2 yellow3)
                  (let ([v (make-vector 68 '())])
                    (vector-set! v 25 (list green0))
                    v)
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green (make-vector 7 '()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  '())
      20)
(test (board-move-piece-main (make-board (list blue0 blue1 blue2 blue3
                                               green1 green2 green3 
                                               red0 red1 red2 red3 
                                               yellow0 yellow1 yellow2 yellow3)
                                         (let ([v (make-vector 68 '())])
                                           (vector-set! v 25 (list green0))
                                           v)
                                         (list (cons 'blue (make-vector 7 '()))
                                               (cons 'green (make-vector 7 '()))
                                               (cons 'red (make-vector 7 '()))
                                               (cons 'yellow (make-vector 7 '())))
                                         '())
                             green0
                             25
                             20)
      (make-board (list blue0 blue1 blue2 blue3
                        green1 green2 green3 
                        red0 red1 red2 red3 
                        yellow0 yellow1 yellow2 yellow3)
                  (let ([v (make-vector 68 '())])
                    (vector-set! v 45 (list green0))
                    v)
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green (make-vector 7 '()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  '())
      #f)
  
(test (make-moves (make-board (list blue0 blue1 blue2 blue3
                                    green1 green2 green3 
                                    red1 red2 red3 
                                    yellow0 yellow1 yellow2 yellow3)
                              (let ([v (make-vector 68 '())])
                                (vector-set! v 21 (list green0))
                                (vector-set! v 25 (list red0))
                                v)
                              (list (cons 'blue (make-vector 7 '()))
                                    (cons 'green (make-vector 7 '()))
                                    (cons 'red (make-vector 7 '()))
                                    (cons 'yellow (make-vector 7 '())))
                              '())
                  (list (make-move-piece-main green0 21 4)
                        (make-move-piece-main green0 25 20)))
      (make-board (list blue0 blue1 blue2 blue3
                        green1 green2 green3 
                        red0 red1 red2 red3 
                        yellow0 yellow1 yellow2 yellow3)
                  (let ([v (make-vector 68 '())])
                    (vector-set! v 45 (list green0))
                    v)
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green (make-vector 7 '()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  '())
      (list 20))
  
;; blockade on entry
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)])
  (test (board-enter-piece board2 green1)
        (make-board (list blue0 blue1 blue2 blue3
                          green2 green3
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () (,green0 ,green1) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; two enter, one moves
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)])
  (test (board-move-piece-main board3 green0 5 1)
        (make-board (list blue0 blue1 blue2 blue3
                          green2 green3
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () (,green1) (,green0) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; form blockade after move
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)]
              [(board4 bonus4) (board-move-piece-main board3 green0 5 1)])
  (test (board-move-piece-main board4 green1 5 1)
        (make-board (list blue0 blue1 blue2 blue3
                          green2 green3
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () () (,green0 ,green1) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; move to highest numbered spot on the board
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)])
  (test (board-move-piece-main board2 green0 5 62)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () () () () 
                            ()
                            () () () () () () () () 
                            () () () () () () () () 
                            ()
                            () () () () () () () () 
                            () () () () () () () () 
                            ()
                            () () () () () () () () 
                            () () () () () () () () 
                            ()
                            () () () () () () () (,green0))
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; move piece all the way around to the zero-th spot on the board
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 62)])
  (test (board-move-piece-main board3 green0 67 1)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68((,green0) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; move past zero-th spot onto exit ramp
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 62)]
              [(board4 bonus4) (board-move-piece-main board3 green0 67 1)])
  (test (board-move-piece-main board4 green0 0 1)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green `#7((,green0) ()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; move to last spot on home row
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 62)]
              [(board4 bonus4) (board-move-piece-main board3 green0 67 1)])
  (test (board-move-piece-main board4 green0 0 7)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green `#7(() () () () () () (,green0)))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; move home
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 62)]
              [(board4 bonus4) (board-move-piece-main board3 green0 67 1)])
  (test (board-move-piece-main board4 green0 0 8)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green #7(()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    (list green0))
        10))
  
;; move onto home row directly
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)])
  (test (board-move-piece-main board2 green0 5 64)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green `#7((,green0) ()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; home row move forward
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 64)])
  (test (board-move-piece-home board3 green0 0 1)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green `#7(() (,green0) ()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; home row form blockade
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 64)]
              [(board4 bonus4) (board-move-piece-home board3 green0 0 1)]
              [(board5 bonus5) (board-enter-piece board4 green1)]
              [(board6 bonus6) (board-move-piece-main board5 green1 5 64)])
  (test (board-move-piece-home board6 green1 0 1)
        (make-board (list blue0 blue1 blue2 blue3
                          green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green `#7(() (,green0 ,green1) ()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())
        #f))
  
;; home row move front of blockade forward
(test (board-move-piece-home
       (make-board (list blue0 blue1 blue2 blue3
                         green0 green1 green2 green3 
                         red0 red1 red2 red3)
                   (make-vector (* 17 4) '())
                   (list (cons 'blue (make-vector 7 '()))
                         (cons 'green (make-vector 7 '()))
                         (cons 'red (make-vector 7 '()))
                         (cons 'yellow `#7((,yellow0 ,yellow1) (,yellow2 ,yellow3) ())))
                   '())
       yellow3
       1
       2)
      (make-board (list blue0 blue1 blue2 blue3
                        green0 green1 green2 green3 
                        red0 red1 red2 red3)
                  (make-vector (* 17 4) '())
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green (make-vector 7 '()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow `#7((,yellow0 ,yellow1) (,yellow2) () (,yellow3) ())))
                  '())
      #f)
  
;; move the other piece in the front home row blockade forward
(test (board-move-piece-home
       (make-board (list blue0 blue1 blue2 blue3
                         green0 green1 green2 green3 
                         red0 red1 red2 red3)
                   (make-vector (* 17 4) '())
                   (list (cons 'blue (make-vector 7 '()))
                         (cons 'green (make-vector 7 '()))
                         (cons 'red (make-vector 7 '()))
                         (cons 'yellow `#7((,yellow0 ,yellow1) (,yellow2 ,yellow3) ())))
                   '())
       yellow2
       1
       2)
      (make-board (list blue0 blue1 blue2 blue3
                        green0 green1 green2 green3 
                        red0 red1 red2 red3)
                  (make-vector (* 17 4) '())
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green (make-vector 7 '()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow `#7((,yellow0 ,yellow1) (,yellow3) () (,yellow2) ())))
                  '())
      #f)
  
(test (board-move-piece-main (mb (make-enter-piece (make-pawn 'yellow 0))
                                 (make-move-piece-main (make-pawn 'yellow 0) 56 65)
                                 (make-move-piece-home (make-pawn 'yellow 0) 1 6)
                                 (make-enter-piece (make-pawn 'yellow 1))
                                 (make-move-piece-main (make-pawn 'yellow 1) 56 63)
                                 (make-enter-piece (make-pawn 'yellow 2))
                                 (make-move-piece-main (make-pawn 'yellow 2) 56 63)
                                 (make-enter-piece (make-pawn 'yellow 3))
                                 (make-move-piece-main (make-pawn 'yellow 3) 56 62))
                             (make-pawn 'yellow 1)
                             51
                             2)
      (mb (make-enter-piece (make-pawn 'yellow 0))
          (make-move-piece-main (make-pawn 'yellow 0) 56 65)
          (make-move-piece-home (make-pawn 'yellow 0) 1 6)
          (make-enter-piece (make-pawn 'yellow 1))
          (make-move-piece-main (make-pawn 'yellow 1) 56 63)
          (make-move-piece-main (make-pawn 'yellow 1) 51 2)
          (make-enter-piece (make-pawn 'yellow 2))
          (make-move-piece-main (make-pawn 'yellow 2) 56 63)
          (make-enter-piece (make-pawn 'yellow 3))
          (make-move-piece-main (make-pawn 'yellow 3) 56 62))
      #f)
  
(test (board-move-piece-main (mb (make-enter-piece (make-pawn 'yellow 0))
                                 (make-move-piece-main (make-pawn 'yellow 0) 56 65)
                                 (make-move-piece-home (make-pawn 'yellow 0) 1 6)
                                 (make-enter-piece (make-pawn 'yellow 1))
                                 (make-move-piece-main (make-pawn 'yellow 1) 56 63)
                                 (make-enter-piece (make-pawn 'yellow 2))
                                 (make-move-piece-main (make-pawn 'yellow 2) 56 63)
                                 (make-enter-piece (make-pawn 'yellow 3))
                                 (make-move-piece-main (make-pawn 'yellow 3) 56 62))
                             (make-pawn 'yellow 2)
                             51
                             2)
      (mb (make-enter-piece (make-pawn 'yellow 0))
          (make-move-piece-main (make-pawn 'yellow 0) 56 65)
          (make-move-piece-home (make-pawn 'yellow 0) 1 6)
          (make-enter-piece (make-pawn 'yellow 1))
          (make-move-piece-main (make-pawn 'yellow 1) 56 63)
          (make-enter-piece (make-pawn 'yellow 2))
          (make-move-piece-main (make-pawn 'yellow 2) 56 63)
          (make-move-piece-main (make-pawn 'yellow 2) 51 2)
          (make-enter-piece (make-pawn 'yellow 3))
          (make-move-piece-main (make-pawn 'yellow 3) 56 62))
      #f)
  
(test (board-move-piece-main
       (make-board (list blue0 blue2 blue1 blue3
                         green0
                         red0 red1 red2 red3
                         yellow0 yellow2 yellow3)
                   (let ([main (make-vector 68 '())])
                     (vector-set! main 66 (list (make-pawn 'green 3)))
                     main)
                   (list (cons 'blue (make-vector 7 '()))
                         (cons 'green `#7(()))
                         (cons 'red (make-vector 7 '()))
                         (cons 'yellow (make-vector 7 '())))
                   '())
       green3
       66
       10)
      (make-board (list blue0 blue2 blue1 blue3
                        green0
                        red0 red1 red2 red3
                        yellow0 yellow2 yellow3)
                  (make-vector 68 '())
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green `#7(()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  (list green3))
      10)
  
(test (board-move-piece-main
       (make-board (list blue0 blue1 blue2 blue3
                         green0
                         red0 red1 red2 red3
                         yellow0 yellow2 yellow3)
                   (let ([main (make-vector 68 '())])
                     (vector-set! main 56 (list (make-pawn 'green 3)))
                     main)
                   (list (cons 'blue (make-vector 7 '()))
                         (cons 'green `#7(()))
                         (cons 'red (make-vector 7 '()))
                         (cons 'yellow (make-vector 7 '())))
                   '())
       green3
       56
       20)
      (make-board (list blue0 blue1 blue2 blue3
                        green0
                        red0 red1 red2 red3
                        yellow0 yellow2 yellow3)
                  (make-vector 68 '())
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green `#7(()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  (list green3))
      10)
  
;; main board move front of blockade forward
(test (board-move-piece-main
       (make-board (list blue0 blue1 blue2 blue3
                         green0 green1 green2 green3 
                         red0 red1 red2 red3)
                   `#68(() (,yellow1 ,yellow0) ())
                   (list (cons 'blue (make-vector 7 '()))
                         (cons 'green (make-vector 7 '()))
                         (cons 'red (make-vector 7 '()))
                         (cons 'yellow (make-vector 7 '())))
                   '())
       yellow0
       1
       2)
      (make-board (list blue0 blue1 blue2 blue3
                        green0 green1 green2 green3 
                        red0 red1 red2 red3)
                  `#68(() (,yellow1) () (,yellow0) ())
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green (make-vector 7 '()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  '())
      #f)
  
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 60)]
              [(board4 bonus4) (board-move-piece-main board3 green0 65 10)])
  (test (board-move-piece-home board4 green0 6 1)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    (list green0))
        10))
  
;; enter three on main space without moving
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)])
  (test-err (board-enter-piece board3 green2)
            exn:bad-move?))
  
;; move non-existent piece
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)])
  (test-err (board-move-piece-main board2 green0 4 1)
            exn:bad-move?))
  
;; move non-existent piece, 2
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)])
  (test-err (board-move-piece-main board2 green0 6 1)
            exn:bad-move?))
  
;; move non-existent piece, 2
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)])
  (test-err (board-move-piece-main board2 green1 5 1)
            exn:bad-move?))
  
;; move onto blockade
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 red0)]
              [(board3 bonus3) (board-enter-piece board2 red1)]
              [(board4 bonus4) (board-enter-piece board3 green0)])
  (test-err (board-move-piece-main board4 green0 5 17)
            exn:bad-move?))
  
;; move onto another blockade
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)]
              [(board4 bonus4) (board-move-piece-main board3 green0 5 18)]
              [(board5 bonus5) (board-move-piece-main board4 green1 5 18)]
              [(board6 bonus6) (board-enter-piece board5 red0)])
  (test-err (board-move-piece-main board6 red0 22 1)
            exn:bad-move?))
  
;; move onto a blockade in the home row from the main area
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 64)]
              [(board4 bonus4) (board-move-piece-home board3 green0 0 1)]
              [(board5 bonus5) (board-enter-piece board4 green1)]
              [(board6 bonus6) (board-move-piece-main board5 green1 5 64)]
              [(board7 bonus7) (board-move-piece-home board6 green1 0 1)]
              [(board8 bonus8) (board-enter-piece board7 green2)]
              [(board9 bonus9) (board-move-piece-main board8 green2 5 60)])
  (test-err (board-move-piece-main board9 green2 65 5) exn:bad-move?))
  
;; move past blockade
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 red0)]
              [(board3 bonus3) (board-enter-piece board2 red1)]
              [(board4 bonus4) (board-enter-piece board3 green0)])
  (test-err (board-move-piece-main board4 green0 5 18)
            exn:bad-move?))
  
;; move past home (tests modular arithmetic)
(test-err (board-move-piece-main 
           (make-board (list blue2 blue3)
                       `#68((,yellow0) (,yellow1) (,blue0) (,blue1) () 
                                       (,green0 ,green1) () () () () () () () 
                                       (,green2) () () () 
                                       (,red0) () () () () 
                                       (,red1 ,red2) () () () () 
                                       (,yellow2) () () () () () () () () () () () () ()
                                       (,green3) () () () () () () () () 
                                       (,yellow3) () () () () 
                                       (,red2) ())
                       '((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(())))
                       '())
           yellow3
           50
           20)
          exn:bad-move?)
  
  
;; move onto blockade on the home row
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 64)]
              [(board4 bonus4) (board-move-piece-home board3 green0 0 1)]
              [(board5 bonus5) (board-enter-piece board4 green1)]
              [(board6 bonus6) (board-move-piece-main board5 green1 5 64)]
              [(board7 bonus7) (board-move-piece-home board6 green1 0 1)]
              [(board8 bonus8) (board-enter-piece board7 green2)]
              [(board9 bonus9) (board-move-piece-main board8 green2 5 64)])
  (test-err (board-move-piece-home board9 green2 0 1)
            exn:bad-move?))
  
;; move past blockade on the home row
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 64)]
              [(board4 bonus4) (board-move-piece-home board3 green0 0 1)]
              [(board5 bonus5) (board-enter-piece board4 green1)]
              [(board6 bonus6) (board-move-piece-main board5 green1 5 64)]
              [(board7 bonus7) (board-move-piece-home board6 green1 0 1)]
              [(board8 bonus8) (board-enter-piece board7 green2)]
              [(board9 bonus9) (board-move-piece-main board8 green2 5 64)])
  (test-err (board-move-piece-home board9 green2 0 2)
            exn:bad-move?))
  
;; okay to move in home row with a blockade in a different home row
(test (board-move-piece-home (make-board '()
                                         #68(())
                                         (list (cons 'blue `#7(() (,blue0 ,blue1) ()))
                                               (cons 'green `#7((,green0) ()))
                                               (cons 'red (make-vector 7 '()))
                                               (cons 'yellow (make-vector 7 '())))
                                         '())
                             green0
                             0
                             3)
      (make-board '()
                  #68(())
                  (list (cons 'blue `#7(() (,blue0 ,blue1) ()))
                        (cons 'green `#7(() () () (,green0) ()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  '())
      #f)
  
;; okay to move in home row with a blockade in a different home row, 2
(test (board-move-piece-home (make-board '()
                                         #68(())
                                         (list (cons 'blue `#7(() (,blue0 ,blue1) ()))
                                               (cons 'green `#7((,green0) ()))
                                               (cons 'red (make-vector 7 '()))
                                               (cons 'yellow (make-vector 7 '())))
                                         '())
                             green0
                             0
                             1)
      (make-board '()
                  #68(())
                  (list (cons 'blue `#7(() (,blue0 ,blue1) ()))
                        (cons 'green `#7(() (,green0) ()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  '())
      #f)
  
;; okay to move in home row with a blockade in a different home row, but going home
(test (board-move-piece-home (make-board '()
                                         #68(())
                                         (list (cons 'blue `#7(() (,blue0 ,blue1) ()))
                                               (cons 'green `#7((,green0) ()))
                                               (cons 'red (make-vector 7 '()))
                                               (cons 'yellow (make-vector 7 '())))
                                         '())
                             green0
                             0
                             7)
      (make-board '()
                  #68(())
                  (list (cons 'blue `#7(() (,blue0 ,blue1) ()))
                        (cons 'green `#7(()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  (list green0))
      10)
  
;; moving onto a blockade at 0
(test-err (board-move-piece-home (make-board '()
                                             (let ([v (vector-copy #68(()))])
                                               (vector-set! v 0 (list blue0 blue1))
                                               (vector-set! v 67 (list red0))
                                               v)
                                             (list (cons 'blue `#7(()))
                                                   (cons 'green `#7(()))
                                                   (cons 'red (make-vector 7 '()))
                                                   (cons 'yellow (make-vector 7 '())))
                                             '())
                                 red0
                                 67
                                 1)
          exn:bad-move?)
  
;; moving past a blockade at 0
(test-err (board-move-piece-home (make-board '()
                                             (let ([v (vector-copy #68(()))])
                                               (vector-set! v 0 (list blue0 blue1))
                                               (vector-set! v 67 (list red0))
                                               v)
                                             (list (cons 'blue `#7(()))
                                                   (cons 'green `#7(()))
                                                   (cons 'red (make-vector 7 '()))
                                                   (cons 'yellow (make-vector 7 '())))
                                             '())
                                 red0
                                 67
                                 2)
          exn:bad-move?)
  
;; moving onto a blockade at 67
(test-err (board-move-piece-home (make-board '()
                                             (let ([v (vector-copy #68(()))])
                                               (vector-set! v 67 (list blue0 blue1))
                                               (vector-set! v 66 (list red0))
                                               v)
                                             (list (cons 'blue `#7(()))
                                                   (cons 'green `#7(()))
                                                   (cons 'red (make-vector 7 '()))
                                                   (cons 'yellow (make-vector 7 '())))
                                             '())
                                 red0
                                 66
                                 1)
          exn:bad-move?)
  
;; moving past a blockade at 67
(test-err (board-move-piece-home (make-board '()
                                             (let ([v (vector-copy #68(()))])
                                               (vector-set! v 67 (list blue0 blue1))
                                               (vector-set! v 66 (list red0))
                                               v)
                                             (list (cons 'blue `#7(()))
                                                   (cons 'green `#7(()))
                                                   (cons 'red (make-vector 7 '()))
                                                   (cons 'yellow (make-vector 7 '())))
                                             '())
                                 red0
                                 66
                                 2)
          exn:bad-move?)
  
;; impossible to move, due to blockade
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)]
              [(board4 bonus4) (board-move-piece-main board3 green0 5 18)]
              [(board5 bonus5) (board-move-piece-main board4 green1 5 18)]
              [(board6 bonus6) (board-enter-piece board5 red0)])
  (test (possible-to-move 'red board6 '1)
        #f))

;; enter with a 5
(test-take-turn 'green (new-board) '(5) (list (make-enter-piece green0))
                (make-board (list blue0 blue1 blue2 blue3
                                  green1 green2 green3 
                                  red0 red1 red2 red3 
                                  yellow0 yellow1 yellow2 yellow3)
                            `#68(() () () () () (,green0) ())
                            (list (cons 'blue (make-vector 7 '()))
                                  (cons 'green (make-vector 7 '()))
                                  (cons 'red (make-vector 7 '()))
                                  (cons 'yellow (make-vector 7 '())))
                            '()))
  
;; enter with a 1 and a 4
(test-take-turn 'green (new-board) '(1 4) (list (make-enter-piece green0))
                (make-board (list blue0 blue1 blue2 blue3
                                  green1 green2 green3 
                                  red0 red1 red2 red3 
                                  yellow0 yellow1 yellow2 yellow3)
                            `#68(() () () () () (,green0) ())
                            (list (cons 'blue (make-vector 7 '()))
                                  (cons 'green (make-vector 7 '()))
                                  (cons 'red (make-vector 7 '()))
                                  (cons 'yellow (make-vector 7 '())))
                            '()))
  
;; enter with a 2 and a 3
(test-take-turn 'green (new-board) '(3 2) (list (make-enter-piece green0))
                (make-board (list blue0 blue1 blue2 blue3
                                  green1 green2 green3
                                  red0 red1 red2 red3 
                                  yellow0 yellow1 yellow2 yellow3)
                            `#68(() () () () () (,green0) ())
                            (list (cons 'blue (make-vector 7 '()))
                                  (cons 'green (make-vector 7 '()))
                                  (cons 'red (make-vector 7 '()))
                                  (cons 'yellow (make-vector 7 '())))
                            '()))
  
;; enter with two fives
(test-take-turn 'green (new-board) '(5 5) (list (make-enter-piece green0) (make-enter-piece green1))
                (make-board (list blue0 blue1 blue2 blue3
                                  green2 green3
                                  red0 red1 red2 red3 
                                  yellow0 yellow1 yellow2 yellow3)
                            `#68(() () () () () (,green0 ,green1) ())
                            (list (cons 'blue (make-vector 7 '()))
                                  (cons 'green (make-vector 7 '()))
                                  (cons 'red (make-vector 7 '()))
                                  (cons 'yellow (make-vector 7 '())))
                            '()))
  
;; cannot take either die, due to blockade
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)]
              [(board4 bonus4) (board-move-piece-main board3 green0 5 18)]
              [(board5 bonus5) (board-move-piece-main board4 green1 5 18)]
              [(board6 bonus6) (board-enter-piece board5 red0)])
  (test-take-turn 'red board6 '(1 2) '()
                  (make-board (list blue0 blue1 blue2 blue3
                                    green2 green3
                                    red1 red2 red3
                                    yellow0 yellow1 yellow2 yellow3)
                              `#68(() () () () () () () () () () () () ()
                                      () () () () () () () () () 
                                      (,red0) (,green0 ,green1) ())
                              (list (cons 'blue (make-vector 7 '()))
                                    (cons 'green (make-vector 7 '()))
                                    (cons 'red (make-vector 7 '()))
                                    (cons 'yellow (make-vector 7 '())))
                              '())))
  
;; bop twice and take the bonuses
(test-take-turn
 'yellow
 (make-board (list blue0 blue1 blue2 blue3
                   green2 green3 
                   red0 red1 red2 red3)
             `#68((,yellow0) (,yellow1) () (,yellow2) (,yellow3) () (,green0) (,green1) ())
             (list (cons 'blue (make-vector board-home-row-size '()))
                   (cons 'green (make-vector board-home-row-size '()))
                   (cons 'red (make-vector board-home-row-size '()))
                   (cons 'yellow (make-vector board-home-row-size '())))
             '())
 '(2 4)
 (list (make-move-piece-main yellow2 3 4)
       (make-move-piece-main yellow3 4 2)
       (make-move-piece-main yellow1 1 20)
       (make-move-piece-main yellow0 0 20))
 (make-board (list blue0 blue1 blue2 blue3
                   green0 green1 green2 green3 
                   red0 red1 red2 red3)
             `#68(() () () () () () (,yellow3) (,yellow2)
                     () () () () () () () () () () () ()
                     (,yellow0) (,yellow1) ())
             (list (cons 'blue (make-vector board-home-row-size '()))
                   (cons 'green (make-vector board-home-row-size '()))
                   (cons 'red (make-vector board-home-row-size '()))
                   (cons 'yellow (make-vector board-home-row-size '())))
             '()))
  
;; is okay to enter after taking the 3 to break up blockade
(test-take-turn 'red
                (make-board
                 (list blue0 blue1 blue2 
                       red0)
                 `#68((,yellow0 ,yellow1) 
                      ()
                      (,red1) () ()
                      (,green0 ,green1) ()
                      (,yellow2) () () () () () () () () ()
                      (,green2) () () ()
                      (,green3)
                      (,red2 ,red3) () () () () () () () () () () () () 
                      () () () () () () () () () () () () () () () () 
                      (,blue3) () () () () () () () () 
                      (,yellow3) ())
                 '((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(())))
                 '())
                '(3 5)
                (list (make-move-piece-main red2 22 3)
                      (make-enter-piece red0))
                (make-board
                 (list blue0 blue1 blue2)
                 `#68((,yellow0 ,yellow1) 
                      ()
                      (,red1) () ()
                      (,green0 ,green1) ()
                      (,yellow2) () () () () () () () () ()
                      (,green2) () () ()
                      (,green3)
                      (,red0 ,red3) () () (,red2) () () () () () () () () () 
                      () () () () () () () () () () () () () () () () 
                      (,blue3) () () () () () () () () 
                      (,yellow3) ())
                 '((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(())))
                 '()))
  
;; if taking the 5 to break up blocakde first, then entering is not mandated
(test-take-turn 'red
                (make-board
                 (list blue0 blue1 blue2 
                       red0)
                 `#68((,yellow0 ,yellow1)
                      ()
                      (,red1) () ()
                      (,green0 ,green1) ()
                      (,yellow2) () () () () () () () () ()
                      (,green0) () () ()
                      (,green1)
                      (,red2 ,red3) () () () () () () () () () () () () 
                      () () () () () () () () () () () () () () () () 
                      (,blue3) () () () () () () () () 
                      (,yellow3) ())
                 '((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(())))
                 '())
                '(3 5)
                (list (make-move-piece-main red2 22 5)
                      (make-move-piece-main red2 27 3))
                (make-board
                 (list blue0 blue1 blue2 
                       red0)
                 `#68((,yellow0 ,yellow1)
                      ()
                      (,red1) () ()
                      (,green0 ,green1) ()
                      (,yellow2) () () () () () () () () ()
                      (,green0) () () ()
                      (,green1)
                      (,red3) () () () () () () () (,red2) () () () () 
                      () () () () () () () () () () () () () () () () 
                      (,blue3) () () () () () () () () 
                      (,yellow3) ())
                 '((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(())))
                 '()))
  
;; cannot skip move
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)])
  (test-take-turn 'green board3 '(1 2) '()))
  
;; cannot skip move in home row
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 64)]
              [(board4 bonus4) (board-move-piece-home board3 green0 0 1)])
  (test-take-turn 'green board4 '(1 2) '()))
  
;; another test of skipping
(test-take-turn 'yellow
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1 green2 green3 
                                  red0 red1 red2 red3)
                            (make-vector (* 17 4) '())
                            (list (cons 'blue (make-vector 7 '()))
                                  (cons 'green (make-vector 7 '()))
                                  (cons 'red (make-vector 7 '()))
                                  (cons 'yellow `#7((,yellow0 ,yellow1) (,yellow2 ,yellow3) ())))
                            '())
                '(2 1) 
                '())

;; can take only one die, due to blockade
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)]
              [(board4 bonus4) (board-move-piece-main board3 green0 5 19)]
              [(board5 bonus5) (board-move-piece-main board4 green1 5 19)]
              [(board6 bonus6) (board-enter-piece board5 red0)])
  (test-take-turn 'red board6 '(1 2) (list (make-move-piece-main red0 22 1))
                  (make-board (list blue0 blue1 blue2 blue3
                                    green2 green3 
                                    red1 red2 red3
                                    yellow0 yellow1 yellow2 yellow3)
                              `#68(() () () () () () () () () () () () () () 
                                      () () () () () () () () () (,red0) (,green0 ,green1) ())
                              (list (cons 'blue (make-vector 7 '()))
                                    (cons 'green (make-vector 7 '()))
                                    (cons 'red (make-vector 7 '()))
                                    (cons 'yellow (make-vector 7 '())))
                              '())))
  
;; test that moving past blockade on exit is illegal
(let ([board (make-board
              (list blue0 blue1 blue2 blue3
                    yellow0)
              `#68((,red0 ,red1)
                   () () ()
                   (,yellow1) () () () () () () () () () () () () () () () () ()
                   (,red2) () () () () () () () () () () () () () () () () () () 
                   () () () () () () () () () () () () () () () () () () 
                   (,red3) () () () () () ()
                   (,green0) ())
              `((blue . #7(())) 
                (green . #7(())) 
                (red . #7(())) 
                (yellow . #7(() (,yellow0) () (,yellow1) ())))
              (list green1 green2 green3))])
  (test-err (board-move-piece-main board green0 66 3)
            exn:bad-move?))
  
;; entered without a 5
(test-take-turn 'green (new-board) '(1) (list (make-enter-piece green0)))
  
;; attempt to enter two different colors with two fives
(test-take-turn 'green (new-board) '(5 5)
                (list (make-enter-piece green0) (make-enter-piece red0)))
  
;; leave an entering possibility
(test-take-turn 'green (new-board) '(5 5) (list (make-enter-piece green0)))
  
;; don't move the 3
(test-take-turn 'green (new-board) '(5 3) (list (make-enter-piece green0)))
  
;; illegal to not take extra 20 when bopping
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 red0)]
              [(board4 bonus4) (board-move-piece-main board3 green0 5 23)])
  (test-take-turn 'red board4 '(2 4) (list (make-move-piece-main red0 22 2)
                                           (make-move-piece-main red0 24 4))))
  
;; legal to take the extra 20 when bopping
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 red0)]
              [(board4 bonus4) (board-move-piece-main board3 green0 5 23)])
  (test-take-turn 'red board4 '(2 4) (list (make-move-piece-main red0 22 2)
                                           (make-move-piece-main red0 24 4)
                                           (make-move-piece-main red0 28 20))
                  (make-board (list blue0 blue1 blue2 blue3
                                    green0 green1 green2 green3
                                    red1 red2 red3
                                    yellow0 yellow1 yellow2 yellow3)
                              (let ([v (vector-copy #68(()))])
                                (vector-set! v 48 (list red0))
                                v)
                              (list (cons 'blue (make-vector 7 '()))
                                    (cons 'green (make-vector 7 '()))
                                    (cons 'red (make-vector 7 '()))
                                    (cons 'yellow (make-vector 7 '())))
                              '())))
  
;; illegal to not take extra 10 when going home
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 60)]
              [(board4 bonus4) (board-move-piece-main board3 green0 65 10)]
              [(board5 bonus5) (board-enter-piece board4 green1)])
  (test-take-turn 'green board5 '(1) (list (make-move-piece-home green0 6 1))))
  
;; legal to take the extra 10 when moving home
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 60)]
              [(board4 bonus4) (board-move-piece-main board3 green0 65 10)]
              [(board5 bonus5) (board-enter-piece board4 green1)])
  (test-take-turn 'green board5 '(1) (list (make-move-piece-home green0 6 1)
                                           (make-move-piece-main green1 5 10))
                  (make-board (list blue0 blue1 blue2 blue3
                                    green2 green3
                                    red0 red1 red2 red3 
                                    yellow0 yellow1 yellow2 yellow3)
                              `#68(()()()()()()()()()()()()()()()(,green1)())
                              (list (cons 'blue (make-vector 7 '()))
                                    (cons 'green (make-vector 7 '()))
                                    (cons 'red (make-vector 7 '()))
                                    (cons 'yellow (make-vector 7 '())))
                              (list green0))))
  
(test-take-turn 'blue (make-board
                       (list red0)
                       `#68((,blue0 ,blue1)
                            (,red1) () (,red2) (,blue2) (,green0 ,green1)
                            () () () () () () () () () () () () () 
                            (,green2) () () (,red3) () () () () () () () () 
                            (,yellow0) () () () () () () () () () () () ()
                            () () () () () () () () () () () () (,yellow1)
                            (,blue3) () () () () () () () () (,yellow2) (,green3))
                       '((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(())))
                       (list yellow3))
                '(2 5) (list (make-move-piece-main blue3 57 2)
                             (make-move-piece-main blue3 59 5))
                (make-board (list red0)
                            `#68((,blue0 ,blue1)
                                 (,red1) () (,red2) (,blue2) (,green0 ,green1)
                                 () () () () () () () () () () () () () 
                                 (,green2) () () (,red3) () () () () () () () () 
                                 (,yellow0) () () () () () () () () () () () ()
                                 () () () () () () () () () () () () (,yellow1)
                                 () () () () () () () (,blue3) () (,yellow2) (,green3))
                            '((blue . #7(())) (green . #7(())) (red . #7(())) (yellow . #7(())))
                            (list yellow3)))
  
  
;; blockades cannot move together
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)])
  (test-take-turn 'green board3 '(2 2) (list (make-move-piece-main green0 5 2)
                                             (make-move-piece-main green1 5 2))))
  
;; blockades cannot move together, even with alternating the doubles
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)])
  (test-take-turn 'green board3 '(3 3 4 4) (list (make-move-piece-main green0 5 3)
                                                 (make-move-piece-main green1 5 4)
                                                 (make-move-piece-main green0 8 4)
                                                 (make-move-piece-main green1 9 3))))
  
;; killing and making a new blockade is okay, tho
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-enter-piece board2 green1)]
              [(board4 bonus4) (board-move-piece-main board3 green0 5 1)]
              [(board5 bonus5) (board-enter-piece board4  green2)])
  (test-take-turn 'green board5 '(1 2) (list (make-move-piece-main green2 5 2)
                                             (make-move-piece-main green0 6 1))
                  (make-board (list blue0 blue1 blue2 blue3
                                    green3
                                    red0 red1 red2 red3 
                                    yellow0 yellow1 yellow2 yellow3)
                              `#68(() () () () () (,green1) () (,green0 ,green2) ())
                              (list (cons 'blue (make-vector 7 '()))
                                    (cons 'green (make-vector 7 '()))
                                    (cons 'red (make-vector 7 '()))
                                    (cons 'yellow (make-vector 7 '())))
                              '())))
  
  
;; test that blockade moving check doesn't index past end of array
(test-take-turn 'red
                (make-board (list yellow0 yellow1 yellow2 yellow3)
                            `#68((,blue0)
                                 (,red0) (,blue1) () () 
                                 (,green0) () () 
                                 (,blue2) () () () () () () () 
                                 (,red1) () () () () () () () () () () () () ()
                                 () () () () () () () () () () () () () () () () 
                                 () () () () () () () () () () () () () () () () 
                                 (,green1) () () () () 
                                 (,red2 ,red3))
                            `((blue . #7(() () (,blue3) ()))
                              (green . #7((,green2) ()))
                              (red . #7(()))
                              (yellow . #7(())))
                            (list green3))
                '(2 2 5 5) 
                (list (make-move-piece-main red2 67 2)
                      (make-move-piece-main red1 16 2)
                      (make-move-piece-main red2 1 5)
                      (make-move-piece-main red3 67 5))
                (make-board (list yellow0 yellow1 yellow2 yellow3)
                            `#68((,blue0)
                                 (,red0) (,blue1) () (,red3) 
                                 (,green0) (,red2) () 
                                 (,blue2) () () () () () () () 
                                 () () () () () () () () () () () () () ()
                                 () () () () () () () () () () () () () () () () 
                                 () () () () () () () () () () () () () () () () 
                                 (,green1) () () () () 
                                 ())
                            `((blue . #7(() () (,blue3) ()))
                              (green . #7((,green2) ()))
                              (red . #7((,red1) ()))
                              (yellow . #7(())))
                            (list green3)))
  
;; is this moving a blockade together? (no)
(test-take-turn 'yellow
                 (make-board
                  (list blue0 blue1 blue2 blue3
                        green0 green1 
                        red0 red1 red2 red3)
                  `#68(() (,yellow0 ,yellow1) (,yellow2) () (,yellow3) (,green2 ,green3) ())
                  (list (cons 'blue (make-vector board-home-row-size '()))
                        (cons 'green (make-vector board-home-row-size '()))
                        (cons 'red (make-vector board-home-row-size '()))
                        (cons 'yellow (make-vector board-home-row-size '())))
                  '())
                 '(4 4 3 3)
                 (list (make-move-piece-main yellow0 1 3))
                 (make-board
                  (list blue0 blue1 blue2 blue3
                        green0 green1 
                        red0 red1 red2 red3)
                  `#68(() (,yellow1) (,yellow2) () (,yellow0 ,yellow3) (,green2 ,green3) ())
                  (list (cons 'blue (make-vector board-home-row-size '()))
                        (cons 'green (make-vector board-home-row-size '()))
                        (cons 'red (make-vector board-home-row-size '()))
                        (cons 'yellow (make-vector board-home-row-size '())))
                  '()))
  
;; does not count as moving a blockade together
(test-take-turn 'yellow
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1
                                  red0 red1 red2 red3
                                  yellow0)
                            `#68(() (,yellow1) (,yellow2 ,yellow3) () () (,green2 ,green3) ())
                            (list (cons 'blue (make-vector board-home-row-size '()))
                                  (cons 'green (make-vector board-home-row-size '()))
                                  (cons 'red (make-vector board-home-row-size '()))
                                  (cons 'yellow (make-vector board-home-row-size '())))
                            '())
                '(1 2)
                (list (make-move-piece-main yellow2 2 1)
                      (make-move-piece-main yellow1 1 2))
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1 
                                  red0 red1 red2 red3
                                  yellow0)
                            `#68(() () (,yellow3) (,yellow1 ,yellow2) () (,green2 ,green3) ())
                            (list (cons 'blue (make-vector board-home-row-size '()))
                                  (cons 'green (make-vector board-home-row-size '()))
                                  (cons 'red (make-vector board-home-row-size '()))
                                  (cons 'yellow (make-vector board-home-row-size '())))
                            '()))
  
  
;; cannot take the other 3, since it would mean moving a blocakde together
(test-take-turn 'yellow
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1 
                                  red0 red1 red2 red3
                                  yellow0 yellow1)
                            `#68(() (,yellow2 ,yellow3) () () () (,green2 ,green3) ())
                            (list (cons 'blue (make-vector board-home-row-size '()))
                                  (cons 'green (make-vector board-home-row-size '()))
                                  (cons 'red (make-vector board-home-row-size '()))
                                  (cons 'yellow (make-vector board-home-row-size '())))
                            '())
                '(3 3)
                (list (make-move-piece-main yellow2 1 3))
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1 
                                  red0 red1 red2 red3
                                  yellow0 yellow1)
                            `#68(() (,yellow3) () () (,yellow2) (,green2 ,green3) ())
                            (list (cons 'blue (make-vector board-home-row-size '()))
                                  (cons 'green (make-vector board-home-row-size '()))
                                  (cons 'red (make-vector board-home-row-size '()))
                                  (cons 'yellow (make-vector board-home-row-size '())))
                            '()))
  
;; bop twice and then move a blockade together by 20s
(test-take-turn 'yellow
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1
                                  red0 red1 red2 red3
                                  yellow0 yellow1)
                            `#68(() (,yellow0 ,yellow1) () (,yellow2) (,yellow3)
                                    () (,green2) (,green3) ())
                            (list (cons 'blue (make-vector board-home-row-size '()))
                                  (cons 'green (make-vector board-home-row-size '()))
                                  (cons 'red (make-vector board-home-row-size '()))
                                  (cons 'yellow (make-vector board-home-row-size '())))
                            '())
                '(2 4)
                (list (make-move-piece-main yellow2 3 4)
                      (make-move-piece-main yellow3 4 2)
                      (make-move-piece-main yellow0 1 20)
                      (make-move-piece-main yellow1 1 20)))
  
;; this is not really moving a blockade together
(test-take-turn 'yellow
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1 green2 green3
                                  red0 red1 red2 red3)
                            `#68((,yellow0) (,yellow1) (,yellow2 ,yellow3) () ())
                            (list (cons 'blue (make-vector board-home-row-size '()))
                                  (cons 'green (make-vector board-home-row-size '()))
                                  (cons 'red (make-vector board-home-row-size '()))
                                  (cons 'yellow (make-vector board-home-row-size '())))
                            '())
                '(6 6 1 1)
                (list (make-move-piece-main yellow2 2 6)
                      (make-move-piece-main yellow1 1 1)
                      (make-move-piece-main yellow1 2 6)
                      (make-move-piece-main yellow3 2 1))
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1 green2 green3
                                  red0 red1 red2 red3)
                            `#68((,yellow0) () () (,yellow3) () () () () (,yellow1 ,yellow2) ())
                            (list (cons 'blue (make-vector board-home-row-size '()))
                                  (cons 'green (make-vector board-home-row-size '()))
                                  (cons 'red (make-vector board-home-row-size '()))
                                  (cons 'yellow (make-vector board-home-row-size '())))
                            '()))
  
;; move two in and the move a blockade together by 10s
(test-take-turn 'yellow
                (make-board (list blue0 blue1 blue2 blue3
                                  green0 green1 green2 green3
                                  red0 red1 red2 red3)
                            `#68((,yellow2 ,yellow3) ())
                            (list (cons 'blue (make-vector board-home-row-size '()))
                                  (cons 'green (make-vector board-home-row-size '()))
                                  (cons 'red (make-vector board-home-row-size '()))
                                  (cons 'yellow `#7(() () () () () (,yellow0) (,yellow1))))
                            '())
                '(1 2)
                (list (make-move-piece-home yellow0 5 2)
                      (make-move-piece-home yellow1 6 1)
                      (make-move-piece-main yellow2 0 10)
                      (make-move-piece-main yellow3 0 10)))
  
(test-take-turn 'yellow
                (mb (make-enter-piece (make-pawn 'yellow 0))
                    (make-move-piece-main (make-pawn 'yellow 0) 56 65)
                    (make-move-piece-home (make-pawn 'yellow 0) 1 6)
                    (make-enter-piece (make-pawn 'yellow 1))
                    (make-move-piece-main (make-pawn 'yellow 1) 56 63)
                    (make-enter-piece (make-pawn 'yellow 2))
                    (make-move-piece-main (make-pawn 'yellow 2) 56 63)
                    (make-enter-piece (make-pawn 'yellow 3))
                    (make-move-piece-main (make-pawn 'yellow 3) 56 62))
                '(4 1)
                '())
  
;; simple doubles penalty
(test (board-doubles-penalty (new-board) 'green)
      (new-board))
  
;; doubles penalty on home row
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 62)]
              [(board4 bonus4) (board-move-piece-main board3 green0 67 5)])
  (test (board-doubles-penalty board4 'green)
        (make-board (list blue0 blue1 blue2 blue3
                          green0 green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())))
  
;; doubles penalty just entered
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)])
  (test (board-doubles-penalty board2 'green)
        (make-board (list blue0 blue1 blue2 blue3
                          green0 green1 green2 green3 
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())))
  
;; doubles penalty, prefer home row
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 62)]
              [(board4 bonus4) (board-move-piece-main board3 green0 67 5)]
              [(board5 bonus5) (board-enter-piece board4 green1)])
  (test (board-doubles-penalty board5 'green)
        (make-board (list blue0 blue1 blue2 blue3
                          green0 green2 green3
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () (,green1) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())))
  
;; doubles penalty, prefer piece further along in home row
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 62)]
              [(board4 bonus4) (board-move-piece-main board3 green0 67 5)]
              [(board5 bonus5) (board-enter-piece board4 green1)]
              [(board6 bonus6) (board-move-piece-main board5 green1 5 62)]
              [(board7 bonus7) (board-move-piece-main board6 green1 67 6)])
  (test (board-doubles-penalty board7 'green)
        (make-board (list blue0 blue1 blue2 blue3
                          green1 green2 green3
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    #68(())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green `#7(() () () (,green0) ()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())))
  
  
;; doubles penalty, prefer the piece further along in the board
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 green0)]
              [(board3 bonus3) (board-move-piece-main board2 green0 5 10)]
              [(board4 bonus4) (board-enter-piece board3 green1)]
              [(board5 bonus5) (board-move-piece-main board4 green1 5 9)])
  (test (board-doubles-penalty board5 'green)
        (make-board (list blue0 blue1 blue2 blue3
                          green0 green2 green3
                          red0 red1 red2 red3 
                          yellow0 yellow1 yellow2 yellow3)
                    `#68(() () () () () () () () () () () () () () (,green1) ())
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())))
  
;; doubles penalty -- out of numeric order
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 yellow0)]
              [(board3 bonus3) (board-move-piece-main board2 yellow0 56 14)]
              [(board4 bonus4) (board-enter-piece board3 yellow1)])
  (test (board-doubles-penalty board4 'yellow)
        (make-board (list blue0 blue1 blue2 blue3
                          green0 green1 green2 green3
                          red0 red1 red2 red3 
                          yellow0 yellow2 yellow3)
                    (let ([v (vector-copy #68(() () () ()))])
                      (vector-set! v 56 (list yellow1))
                      v)
                    (list (cons 'blue (make-vector 7 '()))
                          (cons 'green (make-vector 7 '()))
                          (cons 'red (make-vector 7 '()))
                          (cons 'yellow (make-vector 7 '())))
                    '())))
  
(test (board-doubles-penalty (make-board (list blue0 blue1 blue2 blue3
                                               green0 green1 green2 green3
                                               red0 red1 red2 red3 
                                               yellow2 yellow3)
                                         `#68((,yellow0 ,yellow1) ())
                                         (list (cons 'blue (make-vector 7 '()))
                                               (cons 'green (make-vector 7 '()))
                                               (cons 'red (make-vector 7 '()))
                                               (cons 'yellow (make-vector 7 '())))
                                         '())
                             'yellow)
      (make-board (list blue0 blue1 blue2 blue3
                        green0 green1 green2 green3
                        red0 red1 red2 red3 
                        yellow0 yellow2 yellow3)
                  `#68((,yellow0) ())
                  (list (cons 'blue (make-vector 7 '()))
                        (cons 'green (make-vector 7 '()))
                        (cons 'red (make-vector 7 '()))
                        (cons 'yellow (make-vector 7 '())))
                  '()))
  
(test (entering-blockade? (new-board) 'blue)
      #f)
  
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 yellow0)]
              [(board3 bonus3) (board-enter-piece board2 yellow1)])
  (test (entering-blockade? board3 'yellow)
        #t))
  
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 yellow0)]
              [(board3 bonus3) (board-enter-piece board2 yellow1)])
  (test (entering-blockade? board3 'green)
        #f))
  
;; next three make sure that the player isn't penalized for not moving out into a blockade
  
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 (make-pawn 'green 0))]
              [(board3 bonus3) (board-enter-piece board2 (make-pawn 'green 1))]
              [(board4 bonus4) (board-move-piece-main board3 (make-pawn 'green 0) 5 17)]
              [(board5 bonus5) (board-move-piece-main board4 (make-pawn 'green 1) 5 17)])
  (test-take-turn 'red board5 '(2 3) '() board5))
  
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 (make-pawn 'green 0))]
              [(board3 bonus3) (board-enter-piece board2 (make-pawn 'green 1))]
              [(board4 bonus4) (board-move-piece-main board3 (make-pawn 'green 0) 5 17)]
              [(board5 bonus5) (board-move-piece-main board4 (make-pawn 'green 1) 5 17)])
  (test-take-turn 'red board5 '(5 5) '()
                  board5))
  
(let*-values ([(board1) (new-board)]
              [(board2 bonus2) (board-enter-piece board1 (make-pawn 'green 0))]
              [(board3 bonus3) (board-enter-piece board2 (make-pawn 'green 1))]
              [(board4 bonus4) (board-move-piece-main board3 (make-pawn 'green 0) 5 17)]
              [(board5 bonus5) (board-move-piece-main board4 (make-pawn 'green 1) 5 17)])
  (test-take-turn 'red board5 '(1 4) '()
                  board5))

(let ()
  (define board (new-board))
  (define-syntax-rule (move-it t) (begin (define-values (new-board _) t) (set! board new-board)))
  (move-it (board-enter-piece board (make-pawn 'green 0)))
  (move-it (board-move-piece-main board (make-pawn 'green 0) 5 63))
  (move-it (board-move-piece-main board (make-pawn 'green 0) 0 8))

  (move-it (board-enter-piece board (make-pawn 'green 1)))
  (move-it (board-move-piece-main board (make-pawn 'green 1) 5 63))
  (move-it (board-move-piece-main board (make-pawn 'green 1) 0 5))

  (move-it (board-enter-piece board (make-pawn 'green 2)))
  (move-it (board-move-piece-main board (make-pawn 'green 2) 5 63))
  (move-it (board-move-piece-main board (make-pawn 'green 2) 0 5))
  
  (move-it (board-enter-piece board (make-pawn 'green 3)))
  (move-it (board-move-piece-main board (make-pawn 'green 3) 5 63))

  (test-take-turn 'green board '(6 5) '() board))

(test-take-turn 'green
                (make-board (list blue0 blue2 blue1 blue3
                                  green0
                                  red0 red1 red2 red3
                                  yellow0 yellow2 yellow3)
                            (let ([main (make-vector 68 '())])
                              (vector-set! main 66 (list (make-pawn 'green 1) (make-pawn 'green 3)))
                              main)
                            (list (cons 'blue (make-vector 7 '()))
                                  (cons 'green `#7(() () (,(make-pawn 'green 2)) ()))
                                  (cons 'red (make-vector 7 '()))
                                  (cons 'yellow (make-vector 7 '())))
                            '())
                '(2 3)
                (list (make-move-piece-home green2 2 2)
                      (make-move-piece-home green2 4 3)
                      (make-move-piece-main green1 66 10)
                      (make-move-piece-main green3 66 10))
                (make-board (list blue0 blue2 blue1 blue3
                                  green0
                                  red0 red1 red2 red3
                                  yellow0 yellow2 yellow3)
                            (make-vector 68 '())
                            (list (cons 'blue (make-vector 7 '()))
                                  (cons 'green `#7(()))
                                  (cons 'red (make-vector 7 '()))
                                  (cons 'yellow (make-vector 7 '())))
                            `(,green1 ,green2 ,green3)))
  
;; make sure that equal? looks inside the structs 
(test 
 (let ([moves (list (make-enter-piece (make-pawn 'green 0))
                    (make-move-piece-main (make-pawn 'green 0) 5 5))])
   (equal? (let-values ([(board _) (make-moves (new-board) moves)]) board)
           (let-values ([(board _) (make-moves (new-board) moves)]) board)))
 #t)
  
(test-results)
