#|

The paint-by-numbers-canavas% class accepts two initalization
arguments. They must be lists of lists of numbers and they must be the
same length. paint-by-numbers-canvas% objects accepts four methods:

  set-rect : (int int (union 'on 'off 'unknown) -> void)
    Sets the grid point specified by the first two arguments to the third.
    The coordinates are from the top-left and the x coordinate comes first.

  get-rect : (int int ->  (union 'on 'off 'unknown))
    Gets the value of the grid at the coordinates specified by the two integers

  paint-rect : (int int -> void)
    Draws the rectangle specified by the arguments.
    Call this after calling set-rect to see the changes updated on the screen.

  on-paint : (-> void)
    Redraws the entire canvas. May be used if many rects were set.

See the bottom of this file for the creation of a file and a test
paint by numbers.

|#


(require-library "functios.ss")

(define-signature GUI^ (paint-by-numbers-canvas%))

(define GUI
  (unit/sig GUI^

    (import mzlib:function^
	    mred^)

    (define UNKNOWN-BRUSH (send the-brush-list find-or-create-brush "GRAY" 'solid))
    (define ON-BRUSH (send the-brush-list find-or-create-brush "BLUE" 'solid))
    (define OFF-BRUSH (send the-brush-list find-or-create-brush "WHITE" 'solid))
    (define LINES-PEN (send the-pen-list find-or-create-pen "BLACK" 1 'solid))

    (define paint-by-numbers-canvas%
      (class canvas% (parent horiz-numbers vert-numbers)
	(inherit get-dc get-client-size)
	(private
	  [canvas-width 200]
	  [canvas-height 200]
	  
	  [grid-size (length horiz-numbers)]
	  [margin 2]
	  [horiz-label-width 10]
	  [horiz-label-height 10]
	  [vert-label-width 10]
	  [vert-label-height 10]

	  [get-horiz-label-string
	   (lambda (l)
	     (let ([s (format "~a" l)])
	       (substring s 1 (- (string-length s) 1))))]
	  [get-vert-label-strings
	   (lambda (l)
	     (map number->string l))]

	  [grid (build-vector grid-size (lambda (i) (make-vector grid-size UNKNOWN-BRUSH)))]
	  

	  [get-string-height
	   (lambda (s)
	     (let ([dc (get-dc)])
	       (let-values ([(width height descent ascent) 
			     (send dc get-text-extent s)])
		 height)))]
	  [get-string-ascent
	   (lambda (s)
	     (let ([dc (get-dc)])
	       (let-values ([(width height descent ascent) 
			     (send dc get-text-extent s)])
		 ascent)))]
	  [get-string-width
	   (lambda (s)
	     (let ([dc (get-dc)])
	       (let-values ([(width height descent ascent) 
			     (send dc get-text-extent s)])
		 width)))]

	  [xy->grid
	   (lambda (x y)
	     (let* ([grid-width (/ (- canvas-width horiz-label-width) grid-size)]
		    [grid-height (/ (- canvas-height vert-label-height) grid-size)]
		    [xp (- x horiz-label-width)]
		    [yp (- y vert-label-height)]
		    [x (inexact->exact (floor (/ xp grid-width)))]
		    [y (inexact->exact (floor (/ yp grid-height)))])
	       (if (and (<= 0 x grid-size)
			(<= 0 y grid-size))
		   (cons x y)
		   #f)))]
	  
	  
	  [grid->rect
	   (lambda (x y)
	     (let* ([grid-width (- canvas-width horiz-label-width)]
		    [grid-height (- canvas-height vert-label-height)]
		    [left (+ horiz-label-width (* x (/ grid-width grid-size)))]
		    [top (+ vert-label-height (* y (/ grid-height grid-size)))]
		    [width (/ grid-width grid-size)]
		    [height (/ grid-height grid-size)])
	       (values left top width height)))])
	
	
	(public
	  ;; int int -> void
	  [paint-rect
	   (lambda (i j)
	     (let ([dc (get-dc)])
	       (let-values ([(left top width height) (grid->rect i j)])
		 (send dc set-brush (vector-ref (vector-ref grid i) j))
		 (send dc draw-rectangle left top width height))))]

	  ;; int int -> (union 'on 'off 'unknown)
	  [get-rect
	   (lambda (i j)
	     (let ([res (vector-ref (vector-ref grid i) j)])
	       (cond
		[(eq? res UNKNOWN-BRUSH) 'unknown]
		[(eq? res OFF-BRUSH) 'off]
		[(eq? res ON-BRUSH) 'on])))]

	  ;; int int (union 'on 'off 'unknown) -> void
	  [set-rect
	   (lambda (i j sym)
	     (vector-set! (vector-ref grid i)
			  j
			  (case sym
			    [(unknown) UNKNOWN-BRUSH]
			    [(off) OFF-BRUSH]
			    [(on) ON-BRUSH])))])
	

	(override
	 [on-size
	  (lambda (w h)
	    (set! canvas-width w)
	    (set! canvas-height h)
	    (on-paint))]
	 [on-event
	  (lambda (evt)
	    (when (send evt button-down?)
	      (let ([x (send evt get-x)]
		    [y (send evt get-y)])
		(let ([p (xy->grid x y)])
		  (when p
		    (let* ([i (car p)]
			   [j (cdr p)]
			   [prev (vector-ref (vector-ref grid i) j)])
		      (vector-set! (vector-ref grid i) j
				   (cond
				    [(eq? prev UNKNOWN-BRUSH) ON-BRUSH]
				    [(eq? prev ON-BRUSH) OFF-BRUSH]
				    [(eq? prev OFF-BRUSH) UNKNOWN-BRUSH]
				    [else (error 'internal-error "unkown brush in board ~s~n" (vector-ref (vector-ref grid i) j))]))))
		  (on-paint)))))]

	 [on-paint
	  (lambda ()
	    (let ([dc (get-dc)])
	      (let-values ([(width height) (get-client-size)])

		(let loop ([i grid-size])
		  (cond
		   [(zero? i) (void)]
		   [else (let loop ([j grid-size])
			   (cond
			    [(zero? j) (void)]
			    [else (paint-rect (- i 1) (- j 1))
				  (loop (- j 1))]))
			 (loop (- i 1))]))

		
		(let ([square-width (/ (- canvas-width horiz-label-width) grid-size)]
		      [longest-strs (apply max (map length vert-numbers))])
		  (let loop ([l vert-numbers]
			     [n 0])
		    (cond
		     [(null? l) (void)]
		     [else 
		      (let ([strs (get-vert-label-strings (car l))])
			(let loop ([ss strs]
				   [line (- longest-strs (length strs))])
			  (cond
			   [(null? ss) (void)]
			   [else
			    (let* ([s (car ss)]
				   [str-width (get-string-width s)]
				   [str-height (send dc get-char-height)]
				   [x (+ horiz-label-width
					 (* n square-width)
					 (- (/ square-width 2)
					    (/ str-width 2)))]
				   [y (* line (+ str-height margin))])
			      (send dc draw-text (car ss) x y)
			      (loop (cdr ss)
				    (+ line 1)))])))
		      (loop (cdr l) (+ n 1))])))

		(let ([square-height (/ (- canvas-height vert-label-height) grid-size)])
		  (let loop ([l horiz-numbers]
			     [n 0])
		    (cond
		     [(null? l) (void)]
		     [else
		      (let* ([str (get-horiz-label-string (car l))]
			     [str-height (get-string-height str)]
			     [str-ascent (get-string-ascent str)]
			     [str-width (get-string-width str)]
			     [y (+ horiz-label-height
				   (* n square-height)
				   (- (/ square-height 2)
				      (/ str-height 2))
				   str-height)]
			     [x (- horiz-label-width str-width margin)])
			(send dc draw-text str x y))
		      (loop (cdr l)
			    (+ n 1))])))
		
		(void))))])
	
	(inherit min-width min-height)
	(sequence
	  (unless (= (length horiz-numbers) (length vert-numbers))
	    (error 'paint-by-numbers-canvas% 
		   "specifiation lists are not the same length ~e ~e" 
		   horiz-numbers vert-numbers))
	  (super-init parent)
	  
	  (let* ([dc (get-dc)])
	    (set! horiz-label-width
		  (apply max (map (lambda (x) (+ margin (get-string-width (get-horiz-label-string x)) margin))
				  horiz-numbers)))

	    (let-values ([(width height descent ascent) (send dc get-text-extent "0123456789")])
	      (set! horiz-label-height (+ margin margin height)))

	    (set! vert-label-height
		  (apply max
			 (map (lambda (l)
				(let ([strs (get-vert-label-strings l)])
				  (+ (* (- (length strs) 1) margin)
				     (apply + (map get-string-height strs)))))
			      vert-numbers)))
	    (set! vert-label-width
		  (apply max
			 (map (lambda (l)
				(apply max (map get-string-width (get-vert-label-strings l))))
			      vert-numbers))))
	  
	  

	  (min-width (inexact->exact (+ horiz-label-width (* grid-size vert-label-width))))
	  (min-height (inexact->exact (+ vert-label-height (* grid-size horiz-label-height)))))))))

(define MAIN
  (unit/sig ()

    (import GUI^
	    mred^)

    (define horiz-numbers '((1) (2)   (1 6) (9)   (6)   (5) (5) (4) (3) (4)))
    (define vert-numbers  '((2) (1 1) (4)   (2 1) (3 1) (8) (8) (7) (5) (3)))

    (define frame (make-object frame% "Paint by Numbers"))
    (define canvas (make-object paint-by-numbers-canvas% frame horiz-numbers vert-numbers))
    (send frame show #t)))

(invoke-unit/sig
 (compound-unit/sig (import)
   (link
    [F : mzlib:function^ ((require-library "functior.ss"))]
    [M : mred^ (mred@)]
    [G : GUI^ (GUI F M)]
    [MN : () (MAIN G M)])
   (export)))
