(require-library "macro.ss")
(require-library "letplsrc.ss")
(require-library "function.ss")

(require-library "errortrace.ss" "errortrace")

(invoke-unit/sig
 (unit/sig ()
   (import mred^ mzlib:function^)
   
   (define board-width 20)
   (define board-height 10)
   (define cell-size 30)
   (define colors (map (lambda (x) (make-object color% x)) (list "blue" "red" "magenta" "yellow" "cyan")))
   (define pens (map (lambda (x) (make-object pen% x 1 'solid)) colors))
   (define brushes (map (lambda (x) (make-object brush% x 'solid)) colors))
   (define xor-pens (map (lambda (x) (make-object pen% x 1 'xor)) colors))
   (define xor-brushes (map (lambda (x) (make-object brush% x 'xor)) colors))
   (define white-pen (make-object pen% "white" 1 'solid))
   (define white-brush (make-object brush% "white" 'solid))
   
   ;; build-board : (-> (vectorof (vectorof (vector (union num #f) boolean))))
   ;   this represents the board. Each entry is the color index of
   ;   the piece and a node to mark for the depth-first traversal.
   ;   #f for the color index indicates an eliminated piece.
   (define (build-board)
     (build-vector
      board-width
      (lambda (i)
	(build-vector
	 board-height
	 (lambda (j)
	   (vector
	    (begin
	      (if (= j (- board-height 1))
		  (- (length colors) 1)
		  (modulo i (- (length colors) 1)))
	      (random (length colors)))
	    #f))))))

   (define board (build-board))

   (define game-over? #f)

   (define score 0)
   (define (calc-score n)
     (cond
       [(= n 2) 2]
       [else (- (* (- n 1) (- n 1)) (- n 3))]))
   
   (define same-canvas%
     (class-asi canvas%
       (inherit get-dc get-size)
       (private
	 [width #f]
	 [height #f]
	 [x-step #f]
	 [y-step #f])
       (public
	 [draw-cell
	  (lambda (dc highlight? i j)
	    (let ([index (vector-ref (vector-ref (vector-ref board i) j) 0)]
		  [x (* i x-step)]
		  [y (* j y-step)])
	      (cond
		[highlight?
		 (send dc set-pen white-pen)
		 (send dc set-brush white-brush)
		 (send dc draw-ellipse x y x-step y-step)
		 (when index
		   (send dc set-pen (list-ref pens index))
		   (send dc set-brush (list-ref brushes index))
		   (send dc draw-ellipse 
			 (floor (+ x (/ x-step 4)))
			 (floor (+ y (/ y-step 4)))
			 (floor (/ x-step 2))
			 (floor (/ y-step 2))))]
		[else
		 (if index
		     (begin (send dc set-pen (list-ref pens index))
			    (send dc set-brush (list-ref brushes index)))
		     (begin (send dc set-pen white-pen)
			    (send dc set-brush white-brush)))
		 (send dc draw-ellipse x y x-step y-step)])))]
	 
	 [draw-line
	  (lambda (dc i)
	    (let loop ([j board-height])
	      (cond
		[(zero? j) (void)]
		[else
		 (draw-cell dc #f i (- j 1))
		 (loop (- j 1))])))]
	 
	 [find-same-colors
	  (lambda (i j)
	    (let* ([index (vector-ref (vector-ref (vector-ref board i) j) 0)]
		   [ans
		    (let loop ([i i]
			       [j j]
			       [ps null])
		      (cond
			[(not (and (<= 0 i) (< i board-width)
				   (<= 0 j) (< j board-height)))
			 ps]
			[(vector-ref (vector-ref (vector-ref board i) j) 1) ps]
			[(not (vector-ref (vector-ref (vector-ref board i) j) 0)) ps]
			[(= index (vector-ref (vector-ref (vector-ref board i) j) 0))
			 (let ([v (vector-ref (vector-ref board i) j)])
			   (vector-set! v 1 #t)
			   (loop (+ i 1)
				 j
				 (loop (- i 1)
				       j
				       (loop i
					     (- j 1)
					     (loop i
						   (+ j 1)
						   (cons (list v i j) ps))))))]
			[else ps]))])
	      (for-each (lambda (p) (vector-set! (first p) 1 #f)) ans)
	      ans))])

       (public
	 [get-game-over-size
	  (lambda (dc)
	    (let ([border 5]
		  [w (box 0)]
		  [h (box 0)]
		  [d (box 0)]
		  [l (box 0)])
	      (send dc get-text-extent game-over w h d l)
	      (let* ([text-height (unbox h)]
		     [text-width (unbox w)]
		     [x (- (/ width 2) (/ text-width 2))]
		     [y (- (/ height 2) (/ text-height 2))])
		(values x y text-width text-height border))))]

	 [paint-game-over
	  (lambda (dc)
	    (send dc set-font font)
	    (let-values ([(x y text-width text-height border) (get-game-over-size dc)])
	      (send dc set-pen white-pen)
	      (send dc set-brush white-brush)
	      (send dc draw-rectangle
		    (- x border) (- y border)
		    (+ text-width border border)
		    (+ text-height border border))
	      (send dc draw-text game-over x y)))])
	      

       (private
	 [game-over "Game Over"]
	 [font (make-object font% 24 'decorative 'normal 'normal #f)]
	 [turned null])
       
       (override
	[on-size
	 (lambda (w h)
	   (set! width w)
	   (set! height h)
	   (set! x-step (/ width board-width))
	   (set! y-step (/ height board-height)))]
	
	[on-event
	 (lambda (evt)
	   (let+ ([val x (send evt get-x)]
		  [val y (send evt get-y)]
		  [val i (inexact->exact (floor (* (/ x width) board-width)))]
		  [val j (inexact->exact (floor (* (/ y height) board-height)))])
		 (cond
		   [(or (send evt moving?)
			(send evt entering?)
			(send evt leaving?))
		    (cond
		      [(and (<= 0 i) (< i board-width)
			    (<= 0 j) (< j board-height))
		       (unless (member (list i j) turned)
			 (when (> (length turned) 1)
			   (for-each (lambda (p) (draw-cell (get-dc) #f (first p) (second p))) turned))
			 (set! turned (map (lambda (xx) (list (second xx) (third xx))) (find-same-colors i j)))
			 (cond
			   [(> (length turned) 1)
			    (send this-message set-label (number->string (calc-score (length turned))))
			    (for-each (lambda (p) (draw-cell (get-dc) #t (first p) (second p))) turned)]
			   [else
			    (send this-message set-label "")]))]
		       [else
			(when (> (length turned) 1)
			  (for-each (lambda (p) (draw-cell (get-dc) #f (first p) (second p))) turned))
			(set! turned null)
			(send this-message set-label "")])]
		   [(send evt button-up?)
		    (when (and (<= 0 i) (< i board-width)
			       (<= 0 j) (< j board-height))
		      (when (> (length turned) 1)
			(for-each (lambda (p) (draw-cell (get-dc) #f (first p) (second p))) turned))
		      (set! turned null)
		      (send this-message set-label "")
		      (let ([same-colors (find-same-colors i j)])
			
			;; reset back the marks for the next depth-first traversal
			
			(when (>= (length same-colors) 2)
			  
			  ;; update score
			  (set! score (+ score (calc-score (length same-colors))))
			  (send message set-label (number->string score)) 
			  
			  ;; slide down empty pieces
			  (let ([is null])
			    (for-each
			     (lambda (p)
			       (let ([i (second p)]
				     [j (third p)])
				 (unless (member i is)
				   (set! is (cons i is)))
				 (let loop ([x j])
				   (cond
				     [(<= 1 x)
				      (let ([next (vector-ref (vector-ref board i) (- x 1))]
					    [this (vector-ref (vector-ref board i) x)])
					(vector-set! this 0 (vector-ref next 0))
					(loop (- x 1)))]
				     [else
				      (vector-set! (vector-ref (vector-ref board i) x) 0 #f)]))))
			     (quicksort same-colors
					(lambda (x y) (<= (third x) (third y)))))
			    
			    ;; slide empty over empty rows
			    (set! is (quicksort is >))
			    (let ([empty-is (filter (lambda (i)
						      (not (vector-ref (vector-ref (vector-ref board i) (- board-height 1)) 0)))
						    is)])
			      (let ([is (if (null? empty-is)
					    is
					    (filter (lambda (x) (< x (car empty-is)))
						    is))])
				(for-each (lambda (empty-i)
					    (let loop ([i empty-i])
					      (cond
						[(<= i (- board-width 2))
						 (vector-set! board i (vector-ref board (+ i 1)))
						 (loop (+ i 1))]
						[(= i (- board-width 1))
						 (vector-set! board i (build-vector board-height
										    (lambda (i) (vector #f #f))))])))
					  empty-is)
				
				;; draw changed lines
				(for-each (lambda (i) (draw-line (get-dc) i)) is)
				(unless (null? empty-is)
				  (let loop ([i (car (last-pair empty-is))])
				    (cond
				      [(= i board-width) (void)]
				      [else (draw-line (get-dc) i)
					    (loop (+ i 1))]))))))

			  (set! game-over?
				(not
				 (let loop ([i board-width]
					    [continue? #f])
				   (cond
				     [(zero? i) continue?]
				     [else
				      (or continue?
					  (loop
					   (sub1 i)
					   (let loop ([j board-height]
						      [continue? continue?])
					     (cond
					       [(zero? j) continue?]
					       [else
						(or continue?
						    (loop
						     (sub1 j)
						     (> (length (find-same-colors (sub1 i) (sub1 j))) 1)))]))))])))) 
			  (when game-over?
			    (paint-game-over (get-dc))))))]
		     
		     [else (void)])))]
	
	[on-paint
	 (lambda ()
	   (let ([dc (get-dc)])
	     (send dc set-pen white-pen)
	     (send dc set-brush white-brush)
	     (let-values ([(x y width height border) (get-game-over-size dc)])
	       (send dc draw-rectangle
		     (- x border) (- y border)
		     (+ width border border)
		     (+ height border border)))
	     (let loop ([i board-width])
	       (cond
		 [(zero? i) (void)]
		 [else (draw-line dc (- i 1))
		       (loop (- i 1))]))
	     (when game-over?
	       (paint-game-over dc))))])))
   
   (define semaphore (make-semaphore 0))
   (define same-frame%
     (class-asi frame%
       (override
	[on-close
	 (lambda ()
	   (semaphore-post semaphore))])))
   
   (define frame (make-object same-frame% "Same"))
   (define panel (make-object vertical-panel% frame))
   (define canvas (make-object same-canvas% panel))
   (define hp (make-object horizontal-panel% panel))
   (make-object message% "Total Score: " hp)
   (define message (make-object message% "0" hp))
   (make-object message% "This Score: " hp)
   (define this-message (make-object message% "0" hp))
   (define button (make-object button% "New Game" hp
			       (lambda x
				 (set! game-over? #f)
				 (set! board (build-board))
				 (unless (= score 0)
				   (set! score 0)
				   (send message set-label "0"))
				 (send this-message set-label "")
				 (send canvas on-paint))))
   
   (send message stretchable-width #t)
   (send this-message stretchable-width #t)
   (send hp stretchable-height #f)
   (send canvas min-width (* board-width cell-size))
   (send canvas min-height (* board-height cell-size))
   
   (send frame show #t)
   (yield semaphore))
 mred^ mzlib:function^)

