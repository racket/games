
;; This is a simple implementation of the ever-popular Minesweeper game.
;; Ok, the graphics are primitive, and there are no frills, but what
;; do you expect in 400 lines?
;;  -Matthew

(require-library "function.ss")

(define TILE-HW 24)        ; height/width of a tile
(define B-WIDTH 16)        ; number of tiles across
(define B-HEIGHT 16)       ; number of tiles down
(define THE-BOMB-COUNT 30) ; number of bombs to hide

(define DIGIT-COLOR-NAMES
  ; 0th is background; 8th is foreground
  (vector "LIGHT GRAY" 
	  "BLUE" "GREEN" "RED" "PURPLE" 
	  "ORANGE" "YELLOW" "BROWN" "BLACK"))

(define DIGIT-COLORS
  (build-vector 9 (lambda (i)
		    (send the-color-database find-color 
			  (vector-ref DIGIT-COLOR-NAMES i)))))

(define BG-COLOR (vector-ref DIGIT-COLORS 0))
(define FG-COLOR (vector-ref DIGIT-COLORS 8))
(define EXPLODE-COLOR (send the-color-database find-color "RED"))

(define BG-PEN (send the-pen-list find-or-create-pen BG-COLOR 1 'solid))
(define FG-PEN (send the-pen-list find-or-create-pen FG-COLOR 1 'solid))

;; There's a lot of number-based loops below
(define step-while
  (case-lambda 
   [(first test until step f)
    (step-while first test until step f void (void))]
   [(first test until step f accum)
    (step-while first test until step f accum (void))]
   [(first test until step f accum init)
    (let loop ([n first][a init])
      (if (test n until)
	  (loop (step n) (accum a (f n)))
	  a))]))

; Class for a basic tile
(define tile:plain%
  (class object% ()
    (private
      [state 'covered]
      [neighbor-bomb-count 0])
    (public
      [set-state
       (lambda (newstate)
	 (set! state newstate))]
      [get-state
       (lambda ()
	 state)]
      [set-neighbor-bomb-count
       (lambda (c)
	 (set! neighbor-bomb-count c))]
      [get-neighbor-bomb-count
       (lambda ()
	 neighbor-bomb-count)]
      [draw-text-tile
       (lambda (dc x y w h hilite? border? str color)
	 (if border?
	     (send dc set-pen FG-PEN)
	     (send dc set-pen BG-PEN))
	 (send dc draw-rectangle x y w h)
	 (when hilite?
	   (send dc draw-rectangle 
		 (add1 x) (add1 y) 
		 (- w 2) (- h 2)))
	 (when str
	   (if color
	       (send dc set-text-foreground color)
	       (send dc set-text-foreground FG-COLOR))
	   (let-values ([(tw th d a) (send dc get-text-extent str)])
	     (send dc draw-text str 
		   (+ x (/ (- w tw) 2))
		   (+ y (/ (- h th) 2))))))]
      [draw
       (lambda (dc x y w h hilite?)
	 (case state
	   [(covered) (draw-text-tile dc x y w h hilite? #t #f #f)]
	   [(flagged) (draw-text-tile dc x y w h hilite? #t "X" #f)]
	   [(semi-flagged) (draw-text-tile dc x y w h hilite? #t "?" #f)]
	   [(uncovered) (draw-text-tile 
			 dc x y w h #f #f
			 (if (zero? neighbor-bomb-count)
			     #f
			     (number->string neighbor-bomb-count))
			 (vector-ref DIGIT-COLORS neighbor-bomb-count))]))])
    (sequence (super-init))))

; Class for a tile with a bomb underneath
(define tile:bomb%
  (class tile:plain% ()
    (inherit get-state draw-text-tile)
    (rename [super-draw draw])
    (private
      [explode-source? #f])
    (public
      [set-explode-source
       (lambda (s?)
	 (set! explode-source? s?))])
    (override
      [draw
       (lambda (dc x y w h hilite?)
	 (if (eq? (get-state) 'uncovered)
	     (draw-text-tile dc x y w h #f #f "*"
			     (and explode-source? EXPLODE-COLOR))
	     (super-draw dc x y w h hilite?)))])
    (sequence
      (super-init))))

;; A board is a vector of vectors of tiles

(define (get-tile b x y)
  (vector-ref (vector-ref b x) y))

(define (set-tile! b x y t)
  (vector-set! (vector-ref b x) y t))

(define (do-surrounding b x y accum start default f)
  (step-while -1 <= 1 add1
	      (lambda (dx)
		(step-while -1 <= 1 add1
			   (lambda (dy)
			     (if (and (not (and (zero? dx) (zero? dy)))
				      (< -1 (+ x dx) B-WIDTH)
				      (< -1 (+ y dy) B-HEIGHT))
				 (f dx dy)
				 default))
			   accum start))
	      accum start))

(define (is-bomb? x)
  (is-a? x tile:bomb%))

(define (count-surrounding-bombs b x y)
  (do-surrounding 
   b x y + 0 0
   (lambda (dx dy)
     (if (is-bomb? (get-tile b (+ x dx) (+ y dy)))
	 1
	 0))))

(define (for-each-tile b f)
  (step-while 0 < B-WIDTH add1
	      (lambda (x)
		(step-while 0 < B-HEIGHT add1
			    (lambda (y)
			      (f (get-tile b x y) x y))))))

(define (make-board)
  (let ([b (build-vector B-WIDTH
			 (lambda (i)
			   (build-vector B-HEIGHT
					 (lambda (j)
					   (make-object tile:plain%)))))])
    (let loop ([n THE-BOMB-COUNT])
      (unless (zero? n)
	(let rloop ()
	  (let* ([x (random B-WIDTH)]
		 [y (random B-HEIGHT)]
		 [t (get-tile b x y)])
	    (if (is-a? t tile:bomb%)
		(rloop)
		(begin
		  (set-tile! b x y (make-object tile:bomb%))
		  (loop (sub1 n))))))))
    (for-each-tile b (lambda (t x y)
		       (send t
			     set-neighbor-bomb-count
			     (count-surrounding-bombs b x y))))
    b))

;; Most of the work is in this class, which extends the basic canvas
;; class for drawing to the screen. It isn't necessary to put everything
;; in this class, but it's convenient.
(define ms:canvas%
  (class canvas% (vpanel)
    (inherit get-dc min-client-width min-client-height 
	     stretchable-width stretchable-height)
    (private
      [panel (make-object horizontal-panel% vpanel)])
    (sequence
      (send panel stretchable-height #f))
    (private
      [lspace (make-object vertical-panel% panel)]
      [time (make-object message% "Time: 00000" panel)]
      [lmspace (make-object vertical-panel% panel)]
      [button (make-object button% "Reset" panel (lambda (b e) (reset)))]
      [rmspace (make-object vertical-panel% panel)]
      [count (make-object message% "Count: 000" panel)]
      [rspace (make-object vertical-panel% panel)]

      [set-time
       (lambda (t)
	 (send time set-label (string-append "Time: " (number->string t))))]
      [set-count
       (lambda (c)
	 (send count set-label (string-append "Bombs: " (number->string c))))]
      
      [clicking #f]
      [clicking-x 0]
      [clicking-y 0]
      [ready? #t]
      [start-time #f]
      [elapsed-time 0]
      [timer #f]
      [bomb-count THE-BOMB-COUNT]
      [cover-count (* B-HEIGHT B-WIDTH)]
      [board null])
    (public
      (stop-timer
       (lambda ()
	 (when timer
	   (send timer stop)
	   (set! timer #f))))
      (start-timer
       (lambda ()
	 (set! start-time (current-seconds))
	 (set! timer
	       (make-object
		(class timer% ()
		  (override
		    [notify
		     (lambda ()
		       (let ([e (- (current-seconds) start-time)])
			 (when (> e elapsed-time)
			   (set! elapsed-time e)
			   (set-time e))))])
		  (sequence
		    (super-init)))))
	 (send timer start 100 #f)))
      (end-of-game
       (lambda (win?)
	 (stop-timer)
	 (set! ready? #f)
	 (set! start-time #f)
	 (unless win?
	   (show-all-bombs))
	 (set-count THE-BOMB-COUNT)))
      (explode
       (lambda ()
	 (end-of-game #f)))
      (win
       (lambda ()
	 (end-of-game #t)))
      (reset
       (lambda ()
	 (stop-timer)
	 (set! ready? #t)
	 (set! start-time #f)
	 (set! elapsed-time 0)
	 (set! cover-count (* B-HEIGHT B-WIDTH))
	 (send dc clear)
	 (set-time 0)
	 (set! bomb-count THE-BOMB-COUNT)
	 (set-count THE-BOMB-COUNT)
	 (set! board (make-board))
	 (on-paint)))
      (show-all-bombs
       (lambda ()
	 (for-each-tile board
			(lambda (t x y)
			  (when (is-bomb? t)
			    (change-state t (send t get-state) 'uncovered #f)
			    (paint-one t x y))))))
      (autoclick-surrounding 
       (lambda (x y)
	 (do-surrounding 
	  board x y void (void) (void)
	  (lambda (dx dy)
	    (let* ([x2 (+ x dx)]
		   [y2 (+ y dy)]
		   [t (get-tile board x2 y2)]
		   [state (send t get-state)]
		   [nc (send t get-neighbor-bomb-count)])
	      (unless (eq? state 'uncovered)
		(change-state t state 'uncovered #t)
		(paint-one t x2 y2)
		(when (zero? nc)
		  (autoclick-surrounding x2 y2))))))))
      (change-state
       (lambda (t old-state new-state update-count?)
	 (send t set-state new-state)
	 (when (and update-count? (not (eq? new-state old-state)))
	   (when (eq? new-state 'uncovered)
	     (set! cover-count (sub1 cover-count)))
	   (when (eq? old-state 'uncovered)
	     (set! cover-count (add1 cover-count)))
	   (when (eq? new-state 'flagged)
	     (set! bomb-count (sub1 bomb-count))
	     (set-count bomb-count))
	   (when (eq? old-state 'flagged)
	     (set! bomb-count (add1 bomb-count))
	     (set-count bomb-count)))))
      (do-select
       (lambda (x y flag?)
	 (let* ([t (get-tile board x y)]
		[state (send t get-state)]
		[new-state
		 (case state
		   [(covered)
		    (if flag? 'flagged 'uncovered)]
		   [(flagged)
		    (if flag? 'semi-flagged state)]
		   [(semi-flagged)
		    (if flag? 'covered 'uncovered)]
		   [else state])]
		[nc (send t get-neighbor-bomb-count)]
		[new-uncover? (and (eq? new-state 'uncovered)
				   (not (eq? state 'uncovered)))]
		[bomb? (is-bomb? t)])
	   (change-state t state new-state #t)
	   (when (and new-uncover? bomb?)
	     (send t set-explode-source #t))
	   (paint-one t x y)
	   (when new-uncover?
	     (if bomb?
		 (explode)
		 (begin
		   (when (zero? nc)
		     (autoclick-surrounding x y))))
	     (when (and ready? (= cover-count THE-BOMB-COUNT))
	       (win))))))
      (paint-one
       (lambda (t x y)
	 (let ([xloc (* x TILE-HW)]
	       [yloc (* y TILE-HW)])
	   (send t draw dc xloc yloc TILE-HW TILE-HW
		 (eq? t clicking))))))
    (override
      (on-event
       (lambda (e)
	 (when ready?
	   (unless start-time
	     (when (send e button-down?)
	       (start-timer)))
	   (let* ([x (quotient (inexact->exact 
				(floor (send e get-x))) TILE-HW)]
		  [y (quotient (inexact->exact 
				(floor (send e get-y))) TILE-HW)]
		  [t (if (and (< -1 x B-WIDTH)
			      (< -1 y B-HEIGHT))
			 (get-tile board x y)
			 #f)])
	     (cond
	       [(and clicking (or (not (eq? t clicking))
				  (not (or (send e button-up?)
					   (send e dragging?)))))
		(let ([old clicking])
		  (set! clicking #f)
		  (paint-one old clicking-x clicking-y))]
	       [(and t
		     (not (eq? (send t get-state) 'uncovered))
		     (or (send e button-down?)
			 (and (send e dragging?)
			      (= x clicking-x)
			      (= y clicking-y))))
		(set! clicking t)
		(set! clicking-x x)
		(set! clicking-y y)
		(paint-one t x y)]
	       [(send e button-down?) ; click not on a tile
		(set! clicking-x -1)]
	       [(and clicking (send e button-up?))
		(set! clicking #f)
		(do-select x y (send e button-up? 'right))]
	       [else 'ok])))))
      (on-paint
       (lambda ()
	 (for-each-tile board (lambda (t x y)
				(paint-one t x y))))))
    (sequence
      (super-init vpanel)
      (min-client-width (* TILE-HW B-WIDTH))
      (min-client-height (* TILE-HW B-HEIGHT))
      (stretchable-width #f)
      (stretchable-height #f))
    (private
      [dc (get-dc)])
    (sequence
      (reset)
      (send dc set-text-background BG-COLOR)
      (send dc set-brush (send the-brush-list find-or-create-brush 
			       BG-COLOR 'solid)))))

;; Make a frame, install the game, and then show the frame
(define f (make-object frame% "Minesweeper"))
(make-object ms:canvas% f)
(send f show #t)
(yield (make-semaphore))
