(require-library "macro.ss")
(require-library "letplsrc.ss")
(require-library "function.ss")

(invoke-unit/sig
 (unit/sig ()
   (import mred^ mzlib:function^)

   (define board-width 20)
   (define board-height 10)
   (define cell-size 30)
   (define colors (map (lambda (x) (make-object color% x)) (list "blue" "red" "purple" "yellow" "cyan")))
   (define pens (map (lambda (x) (make-object pen% x 1 'solid)) colors))
   (define brushes (map (lambda (x) (make-object brush% x 'solid)) colors))
   (define white-pen (make-object pen% "white" 1 'solid))
   (define white-brush (make-object brush% "white" 'solid))


   ;; board : (vectorof (vectorof (vector (union num #f) boolean)))
   ;   this represents the board. Each entry is the color index of
   ;   the piece and a node to mark for the depth-first traversal.
   ;   #f for the color index indicates an eliminated piece.
   (define board
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
	  (lambda (dc i j)
	    (let ([index (vector-ref (vector-ref (vector-ref board i) j) 0)]
		  [x (* i x-step)]
		  [y (* j y-step)])
	      (if index
		  (begin (send dc set-pen (list-ref pens index))
			 (send dc set-brush (list-ref brushes index)))
		  (begin (send dc set-pen white-pen)
			 (send dc set-brush white-brush)))
	      (send dc draw-ellipse x y x-step y-step)))]

	 [draw-line
	  (lambda (dc i)
	    (let loop ([j board-height])
	      (cond
	       [(zero? j) (void)]
	       [else
		(draw-cell dc i (- j 1))
		(loop (- j 1))])))])

       (override
	 [on-size
	  (lambda (w h)
	    (set! width w)
	    (set! height h)
	    (set! x-step (/ width board-width))
	    (set! y-step (/ height board-height)))]

	 [on-event
	  (lambda (evt)
	    (cond
	     [(send evt button-up?)
	      (let+ ([val x (send evt get-x)]
		     [val y (send evt get-y)]
		     [val i (inexact->exact (floor (* (/ x width) board-width)))]
		     [val j (inexact->exact (floor (* (/ y height) board-height)))]
		     [val index (vector-ref (vector-ref (vector-ref board i) j) 0)])
		    (when (and (<= 0 i board-width)
			       (<= 0 j board-height))
		      (let ([same-colors
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

			;; reset back the marks for the next depth-first traversal
			(for-each (lambda (p) (vector-set! (first p) 1 #f)) same-colors)

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
					   (loop (+ i 1))]))))))))))]
	     
	     [else (void)]))]

	 [on-paint
	  (lambda ()
	    (let ([dc (get-dc)])
	      (let loop ([i board-width])
		(cond
		 [(zero? i) (void)]
		 [else (draw-line dc (- i 1))
		       (loop (- i 1))]))))])))

   (define frame (make-object dialog% "Same"))
   (define panel (make-object vertical-panel% frame))
   (define hp (make-object horizontal-panel% panel))
   (define canvas (make-object same-canvas% panel))
   (make-object message% "Score: " hp)
   (define message (make-object message% "0" hp))

   (send message stretchable-width #t)
   (send hp stretchable-height #f)
   (send canvas min-width (* board-width cell-size))
   (send canvas min-height (* board-height cell-size))

   (send frame show #t))
 mred^ mzlib:function^)

