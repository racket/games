
(module gofish mzscheme
  (require (lib "cards.ss" "games" "cards")
	   (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "unit.ss")
           (lib "etc.ss")
	   (lib "list.ss"))
  
  ;; Player record
  (define-struct player (r hand-r  ; region
                         hand))    ; cards
      
  ;; Initial card count
  (define opponents-count 2)
  (define init-hand-size 7)
      
  ;; Messages
  (define YOUR-NAME "You")
  (define PLAYER-X-NAME "Opponent ~a")
  (define YOUR-TURN-MESSAGE "Your turn.  (Drag a match to your discard box or drag a card to an opponent.)")
  
  ;; Region layout constants
  (define MARGIN 10)
  (define SUBMARGIN 10)
  (define LABEL-H 15)
      
  ;; Randomize
  (random-seed (modulo (current-milliseconds) 10000))
      
  ;; Set up the table
  (define t (make-table "Crazy 8s" 8 5.5))
  (send t create-status-line)
  (send t show #t)
  (send t set-double-click-action #f)
  (send t set-button-action 'left 'drag-raise/one)
  (send t set-button-action 'right 'drag/above)
  
  ;; Get table width & height
  (define w (send t table-width))
  (define h (send t table-height))
      
  ;; Set up the cards
  (define deck (shuffle-list (make-deck) 7))
  (for-each
   (lambda (card)
     (send card user-can-flip #f))
   deck)
      
  ;; Function for dealing or drawing cards
  (define (deal n)
    (let loop ([n n][d deck])
      (if (zero? n)
          (begin
            (set! deck d)
            null)
          (cons (car d) (loop (sub1 n) (cdr d))))))
      
  ;; Card width & height
  (define cw (send (car deck) card-width))
  (define ch (send (car deck) card-height))
  
  (define deck-region
    (make-region (- (/ (- w cw) 2) (/ (+ cw MARGIN) 2))
                 (/ (- h ch) 2)
                 cw ch
                 #f #f))
  (define discard-region
    (make-region (+ (region-x deck-region) cw MARGIN)
                 (region-y deck-region)
                 cw ch
                 #f #f))

  (define discard-target-region
    (make-region (- (region-x discard-region) (/ MARGIN 2))
                 (- (region-y discard-region) (/ MARGIN 2))
                 (+ cw MARGIN) (+ ch MARGIN)
                 "" #f))
  
  (send t add-region discard-target-region)

  ;; Put the cards on the table
  (send t add-cards-to-region deck deck-region)
  
  ;; Player region size
  (define pw (- (/ w opponents-count) (* opponents-count MARGIN)))
  (define ph (- (/ (- h ch) 2) (* 2 MARGIN)))
  
  ;; Define the players with their regions
  (define (make-a-player x y w h lbl)
    (let ([r (make-region x y w h lbl #f)])
      (send t add-region r)
      (make-player
       r
       (make-region (+ x SUBMARGIN) (+ y SUBMARGIN LABEL-H)
                    (- w (* 2 SUBMARGIN))
                    (- h (* 2 SUBMARGIN) LABEL-H)
                    #f #f)
       null)))
  
  (define players
    (cons
     ;; You
     (make-a-player
      (/ MARGIN 2) (- h ph (/ MARGIN 2))
      (- w MARGIN) ph
      YOUR-NAME)
     (build-list
      opponents-count
      (lambda (delta)
        (make-a-player
         (+ (* (+ pw MARGIN) delta) (/ MARGIN 2)) (/ MARGIN 2)
         pw ph
         (format PLAYER-X-NAME (+ 1 delta)))))))
  (define you (car players))
  (define opponents (cdr players))
    
  ;; Card setup: Deal the cards
  (for-each (lambda (player)
              (set-player-hand! player (deal 7))
              (send t move-cards-to-region 
                    (player-hand player)
                    (player-hand-r player)))
            players)

  ;; More card setup: Opponents's cards and deck initally can't be moved
  (for-each
   (lambda (card) (send card user-can-move #f))
   (append
    (apply append
           (map player-hand opponents))
    deck))
  (for-each (lambda (c) (send c home-region (player-r you)))
            (player-hand you))
  
  ;; More card setup: Initial discard
  (define discards (deal 1))
  (send t flip-cards discards)
  (send t move-cards-to-region discards discard-region)
  
  ;; More card setup: Show your cards
  (send t flip-cards (player-hand you))

  ;; Function to enable/disable moving your cards
  (define (enable-your-cards on?)
    (for-each (lambda (c) (send c user-can-move on?))
              (player-hand you)))
      
  ;; Callbacks communicate back to the main loop via these
  (define something-happened (make-semaphore 1))
      
  (define (get-discard-card cs)
    (and (= 1 (length cs))
         (let ([c (car cs)])
           (and (memq c (player-hand you))
                (or (= (send (car discards) get-value)
                       (send c get-value))
                    (= (send (car discards) get-suit-id)
                       (send c get-suit-id))
                    (= (send c get-value) 8))
                c))))
           
  ;; Run the game loop
  (let loop ()
    (when (pair? deck)
      (send (car deck) user-can-move #t)
      (send (car deck) home-region deck-region))
    (set-region-interactive-callback! 
     discard-target-region
     (lambda (in? cs)
       (let ([c (get-discard-card cs)])
         (when c
           (send c home-region (if in? discard-region (player-r you)))))))
    (set-region-callback! 
     discard-target-region
     (lambda (cs)
       (let ([c (get-discard-card cs)])
         (when c
           (send c home-region #f)
           (set! discards (cons c discards))
           (set-player-hand! you (remq c (player-hand you)))
           (send t move-cards-to-region (list c) discard-region)
           (send c user-can-move #f)
           (semaphore-post something-happened)))))
    (set-region-interactive-callback! 
     (player-r you)
     (lambda (in? cs)
       (send (car cs) home-region 
             (if in? (player-r you) deck-region))))
    (set-region-callback! 
     (player-r you)
     (lambda (cs)
       (let ([c (car cs)])
         (send t flip-card c)
         (send c home-region (player-r you))
         (set-player-hand! you (cons c (player-hand you)))
         (set! deck (cdr deck))
         (semaphore-post something-happened))))
    (send t set-status-text YOUR-TURN-MESSAGE)
    (yield something-happened)
    (loop))
    
  )

