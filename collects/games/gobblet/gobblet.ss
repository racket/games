(module gobblet mzscheme
  (require (lib "gl-board.ss" "games" "gl-board-game")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "gl-vectors.ss" "sgl")
           (prefix gl- (lib "sgl.ss" "sgl"))
	   (lib "array.ss" "srfi" "25")
	   (lib "unit.ss"))

  (provide game-unit)
  
  (define yellow (gl-float-vector 1.0 1.0 0.0 1.0))
  (define red (gl-float-vector 1.0 0.0 0.0 1.0))
  (define light-blue (gl-float-vector 0.5 0.5 1.0 1.0))
  (define dark-blue (gl-float-vector 0.0 0.0 1.0 1.0))

  (define-struct piece (size color dl i j))
  
  
  (define game-unit
    (unit
      (import)
      (export)

      (define board-size 3)
      (define piece-sizes (if (= board-size 3)
			      '(0.4 0.6 0.75)
			      '(0.3 0.45 0.65 0.8)))

      (define state (make-array (shape 0 board-size 0 board-size) null))

      (define turn 'red)

      (define (fold-row f v)
	(let iloop ([i 0][v v])
	  (if (= i board-size)
	      v
	      (iloop (add1 i) (f i v)))))

      (define (fold-board f v)
	(fold-row (lambda (i v)
		    (fold-row (lambda (j v)
				(f i j v))
			      v))
		  v))

      (define (move p to)
	(when (piece? p)
	  (let* ((to-x (inexact->exact (floor (gl-vector-ref to 0))))
		 (to-y (inexact->exact (floor (gl-vector-ref to 1))))
		 (from-x (piece-i p))
		 (from-y (piece-j p)))
	    (when (and (<= 0 to-x (sub1 board-size))
		       (<= 0 to-y (sub1 board-size)))
	      (let ([pl (array-ref state to-x to-y)])
		(when (or (null? pl)
			  (< (piece-size (car pl))  (piece-size p)))
		  (when (and from-x from-y)
		    (array-set! state from-x from-y (cdr (array-ref state from-x from-y))))
		  (array-set! state to-x to-y (cons p pl))
		  (send board remove-piece p)
		  (send board add-piece (+ to-x 0.5) (+ to-y 0.5) 0
			(lambda () (gl-call-list (piece-dl p)))
			p)
		  (set-piece-i! p to-x)
		  (set-piece-j! p to-y)
		  (let ([r? (winner? 'red)]
			[y? (winner? 'yellow)])
		    (cond
		     [(and r? y?) (set-winner "Cat")]
		     [r? (set-winner "Red")]
		     [y? (set-winner "Yellow")]
		     [else (set-turn (other turn))]))))))))

      (define (winner? c)
	(let ([row/col
	       (lambda (array-ref)
		 (fold-row (lambda (i v)
			     (or v
				 (fold-row (lambda (j v)
					     (and v
						  (let ([pl (array-ref state i j)])
						    (and (pair? pl)
							 (eq? c (piece-color (car pl)))))))
					   #t)))
			   #f))]
	      [diag
	       (lambda (flip)
		 (fold-row (lambda (i v)
			     (and v
				  (let ([pl (array-ref state i (flip i))])
				    (and (pair? pl)
					 (eq? c (piece-color (car pl)))))))
			   #t))])
	  (or (row/col array-ref)
	      (row/col (lambda (a i j) (array-ref a j i)))
	      (diag (lambda (x) x))
	      (diag (lambda (x) (- board-size 1 x))))))

      (define f (new frame% (label "Gobblet") (width 800) (height 600)))
      (define board
	(new gl-board% (parent f) 
	     (min-x (- 1 board-size)) (max-x (sub1 (* 2 board-size)))
	     (min-y (- 1 board-size)) (max-y (sub1 (* 2 board-size)))
	     (lift 1.2)
	     (move move)))
      (define msg
	(new message% (label "") (parent f) (stretchable-width #t)))

      (define q
	(send board with-gl-context
	      (lambda () (gl-new-quadric))))

      (define space-dl
	(send board with-gl-context
	      (lambda ()
		(let ((list-id (gl-gen-lists 1)))
		  (gl-quadric-draw-style q 'fill)
		  (gl-quadric-normals q 'smooth)
		  (gl-new-list list-id 'compile)
		  (gl-material-v 'front 'ambient-and-diffuse dark-blue)
		  (gl-begin 'polygon)
		  (gl-vertex 0.0 0.0 -0.01)
		  (gl-vertex 1.0 0.0 -0.01)
		  (gl-vertex 1.0 1.0 -0.01)
		  (gl-vertex 0.0 1.0 -0.01)
		  (gl-end)
		  (gl-material-v 'front 'ambient-and-diffuse light-blue)
		  (gl-push-matrix)
		  (gl-translate 0.5 0.5 0.0)
		  (gl-disk q 0.0 .40 25 1)
		  (gl-pop-matrix)
		  (gl-end-list)
		  list-id))))

      (define (make-piece-dl color scale)
	(send board with-gl-context
	      (lambda ()
		(let ((list-id (gl-gen-lists 1)))
		  (gl-quadric-draw-style q 'fill)
		  (gl-quadric-normals q 'smooth)
		  (gl-new-list list-id 'compile)
		  (gl-material-v 'front 'ambient-and-diffuse color)
		  (gl-cylinder q (/ scale 2) (/ scale 2) (* 1.5 scale) 25 1)
		  (gl-push-matrix)
		  (gl-translate 0.0 0.0 (* 1.5 scale))
		  (gl-disk q 0.0 (/ scale 2) 25 1)
		  (gl-pop-matrix)
		  (gl-end-list)
		  list-id))))

      (fold-board (lambda (i j v)
		    (send board add-space
			  (lambda ()
			    (gl-push-matrix)
			    (gl-translate i j 0)
			    (gl-call-list space-dl)
			    (gl-pop-matrix))
			  (cons i j)))
		  void)
      
      (define reds (map (lambda (size)
			  (make-piece-dl red size))
			piece-sizes))

      (define yellows (map (lambda (size)
			     (make-piece-dl yellow size))
			   piece-sizes))

      (define pieces
	(let loop ([reds reds][yellows yellows][sizes piece-sizes] [j 0.5])
	  (if (null? reds)
	      null
	      (append
	       (let ([r (car reds)]
		     [y (car yellows)]
		     [sz (car sizes)])
		 (let loop ([di (- board-size 1.5)])
		   (if (negative? di)
		       null
		       (list*
			(let ([p (make-piece sz 'red r #f #f)])
			  (send board add-piece (- di board-size -1) j 0
				(lambda () (gl-call-list r))
				p)
			  p)
			(let ([p (make-piece sz 'yellow y #f #f)])
			  (send board add-piece (+ board-size di) j 0
				(lambda () (gl-call-list y))
				p)
			  p)
			(loop (sub1 di))))))
	       (loop (cdr reds) (cdr yellows) (cdr sizes) (+ j 1))))))

      (define (set-turn c)
	(set! turn c)
	(send msg set-label (format "~a's turn" (if (eq? turn 'red) "Red" "Yellow")))
	(for-each (lambda (p)
		    (send board enable-piece p (eq? c (piece-color p))))
		  pieces))

      (define (set-winner who)
	(send msg set-label (format "~a wins!" who))
	(for-each (lambda (p)
		    (send board enable-piece p #f))
		  pieces))

      (define (other c)
	(if (eq? c 'red) 'yellow 'red))

      (set-turn turn)

      (send f show #t))))
