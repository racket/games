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

  get-grid : (-> (list-of (list-of (union 'on 'off 'unknown 'wrong))))
    Returns the current state of the entire board as a list of lists.

  set-grid : ((list-of (list-of (union 'on 'off 'unknown 'wrong)))-> void)
    Sets the state of the board. No drawing takes place

  on-paint : (-> void)
    Redraws the entire canvas. May be used if many rects were set.

  all-unknown : (-> void)
    Sets all board positions to 'unknown

See the bottom of this file for the creation of a file and a test
paint by numbers.

|#

(unit/sig GUI^

  (import mzlib:function^
	  mred^)

  (define UNKNOWN-BRUSH (send the-brush-list find-or-create-brush "DARK GRAY" 'solid))
  (define ON-BRUSH (send the-brush-list find-or-create-brush "BLUE" 'solid))
  (define OFF-BRUSH (send the-brush-list find-or-create-brush "WHITE" 'solid))
  (define WRONG-BRUSH (send the-brush-list find-or-create-brush "RED" 'solid))

  (define LINES/NUMBERS-PEN (send the-pen-list find-or-create-pen "BLACK" 1 'solid))

  (define BLACK-PEN (send the-pen-list find-or-create-pen "BLACK" 1 'solid))
  (define WHITE-PEN (send the-pen-list find-or-create-pen "WHITE" 1 'solid))
  (define WHITE-BRUSH (send the-brush-list find-or-create-brush "WHITE" 'solid))

  (define BAR-PEN (send the-pen-list find-or-create-pen "SALMON" 1 'solid))
  (define BAR-BRUSH (send the-brush-list find-or-create-brush "SALMON" 'solid))

  (define-struct do (x y before after))

  (define paint-by-numbers-canvas%
    (class canvas% (parent row-numbers col-numbers)
      (inherit get-dc get-client-size)
      (private
	[longest-strs (apply max (map length col-numbers))]

	[canvas-width 200]
	[canvas-height 200]
	
	[extra-space-every 5]

	[grid-x-size (length col-numbers)]
	[grid-y-size (length row-numbers)]
	[y-margin 1]
	[x-margin 3]
	[row-label-width 10]
	[row-label-height 10]
	[col-label-width 10]
	[col-label-height 10]

	[get-row-label-string
	 (lambda (l)
	   (let ([s (format "~a" l)])
	     (substring s 1 (- (string-length s) 1))))]
	[get-col-label-strings
	 (lambda (l)
	   (map number->string l))]

	[grid (build-vector grid-x-size (lambda (i) (make-vector grid-y-size UNKNOWN-BRUSH)))]
	

	[get-string-height
	 (lambda (s)
	   (let ([dc (get-dc)])
	     (let-values ([(width height descent ascent) 
			   (send dc get-text-extent s)])
	       (- height descent))))]
	[get-string-height/descent
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

	[loc->string
	 (lambda (x y)
	   (format "(~a,~a)" x y))]

	[xy->grid
	 (lambda (x y)
	   (let* ([grid-width (/ (- canvas-width row-label-width) grid-x-size)]
		  [grid-height (/ (- canvas-height col-label-height) grid-y-size)]
		  [xp (- x row-label-width)]
		  [yp (- y col-label-height)]
		  [x (inexact->exact (floor (/ xp grid-width)))]
		  [y (inexact->exact (floor (/ yp grid-height)))])
	     (if (and (<= 0 x)
		      (< x grid-x-size)
		      (<= 0 y)
		      (< y grid-y-size))
		 (cons x y)
		 #f)))]
	
	[grid->rect
	 (lambda (x y)
	   (let* ([grid-width (- canvas-width
				 row-label-width
				 (quotient grid-x-size extra-space-every))]
		  [grid-height (- canvas-height
				  col-label-height
				  (quotient grid-y-size extra-space-every))]
		  [left (+ row-label-width
			   (quotient x extra-space-every)
			   (* x (/ grid-width grid-x-size)))]
		  [top (+ col-label-height
			  (quotient y extra-space-every)
			  (* y (/ grid-height grid-y-size)))]
		  [width (/ grid-width grid-x-size)]
		  [height (/ grid-height grid-y-size)])
	     (values left top width height)))]

	[undo-history null]
	[redo-history null]
	[do-do
	 (lambda (do current-sel new-sel)
	   (let* ([x (do-x do)]
		  [y (do-y do)]
		  [actual (vector-ref (vector-ref grid x) y)]
		  [current (current-sel do)]
		  [new (new-sel do)]
		  [color->val
		   (lambda (brush)
		     (let ([color (send brush get-color)])
		       (list (send color red)
			     (send color green)
			     (send color blue))))])
	     (unless (eq? current actual)
	       (error 'do-do "expected ~a found ~a at (~a,~a)"
		      (color->val current)
		      (color->val actual)
		      x y))
	     (vector-set! (vector-ref grid x) y new)
	     (paint-rect x y)))]

	[brush->symbol
	 (lambda (res)
	   (cond
	    [(eq? res UNKNOWN-BRUSH) 'unknown]
	    [(eq? res OFF-BRUSH) 'off]
	    [(eq? res ON-BRUSH) 'on]
	    [(eq? res WRONG-BRUSH) 'wrong]))]
	[sym->brush
	 (lambda (sym)
	   (case sym
	     [(unknown) UNKNOWN-BRUSH]
	     [(off) OFF-BRUSH]
	     [(on) ON-BRUSH]
	     [(wrong) WRONG-BRUSH]))])
      
      (public

	;; ((list-of (list-of (union 'unknown 'off 'on 'wrong))) -> void)
	[set-grid
	 (lambda (g)
	   (set! undo-history null)
	   (set! redo-history null)
	   (set! grid
		 (list->vector
		  (map (lambda (x) (list->vector (map sym->brush x)))
		       g))))]

	;; (-> (list-of (list-of (union 'unknown 'off 'on 'wrong))))
	[get-grid
	 (lambda ()
	   (map (lambda (x) (map brush->symbol (vector->list x)))
		(vector->list grid)))]

	;; (-> void)
	[undo
	 (lambda ()
	   (cond
	    [(null? undo-history) (bell)]
	    [else
	     (let ([do (car undo-history)])
	       (set! undo-history (cdr undo-history))
	       (set! redo-history (cons do redo-history))
	       (do-do do do-after do-before))]))]

	 ;; (-> void)
	[redo
	 (lambda ()
	   (cond
	    [(null? redo-history) (bell)]
	    [else
	     (let ([do (car redo-history)])
	       (set! redo-history (cdr redo-history))
	       (set! undo-history (cons do undo-history))
	       (do-do do do-before do-after))]))]

	;; (int int -> void)
	[paint-rect
	 (lambda (i j)
	   (let ([dc (get-dc)])
	     (let-values ([(left top width height) (grid->rect i j)])
	       (send dc set-pen LINES/NUMBERS-PEN)
	       (send dc set-brush (vector-ref (vector-ref grid i) j))
	       (send dc draw-rectangle left top width height))))]

	;; (int int -> (union 'on 'off 'unknown 'wrong))
	[get-rect
	 (lambda (i j)
	   (let ([res (vector-ref (vector-ref grid i) j)])
	     (brush->symbol res)))]

	;; (int int (union 'on 'off 'unknown 'wrong) -> void)
	[set-rect
	 (lambda (i j sym)
	   (vector-set! (vector-ref grid i)
			j
			(sym->brush sym)))]


	;; (-> void)
	[all-unknown
	 (lambda ()
	   (let loop ([i grid-x-size])
	     (cond
	      [(zero? i) (void)]
	      [else
	       (let loop ([j grid-y-size])
		 (cond
		  [(zero? j) (void)]
		  [else (set-rect (- i 1) (- j 1) 'unknown)
			(loop (- j 1))]))
	       (loop (- i 1))])))])
      
      (private
	[draw-row-label
	 (lambda (n nums)
	   (let-values ([(gx gy gw gh) (grid->rect 0 n)])
	     (let* ([dc (get-dc)]
		    [str (get-row-label-string nums)]
		    [str-height (get-string-height str)]
		    [str-ascent (get-string-ascent str)]
		    [str-width (get-string-width str)]
		    [sy (+ gy
			   (- (/ gh 2)
			      (/ str-height 2)))]
		    [sx (- row-label-width str-width x-margin)]

		    [x 0]
		    [y gy]
		    [w gx]
		    [h gh])
	       (send dc draw-rectangle x y w h)
	       (send dc draw-text str sx sy))))]
	
	[draw-col-label
	 (lambda (n nums)
	   (let-values ([(gx gy gw gh) (grid->rect n 0)])
	     (let ([strs (get-col-label-strings nums)]
		   [dc (get-dc)])
	       (send dc draw-rectangle gx 0 gw gy)
	       (let loop ([ss strs]
			  [line (- longest-strs (length strs))])
		 (cond
		  [(null? ss) (void)]
		  [else
		   (let* ([s (car ss)]
			  [str-width (get-string-width s)]
			  [str-height (get-string-height s)]
			  [x (+ gx
				(- (/ gw 2)
				   (/ str-width 2)))]
			  [y (* line (+ str-height y-margin))])
		     (send dc draw-text (car ss) x y)
		     (loop (cdr ss)
			   (+ line 1)))])))))]

	[last-p #f])

      (override
       [on-size
	(lambda (w h)
	  (set! canvas-width w)
	  (set! canvas-height h))]
       [on-event
	(lambda (evt)
	  (let* ([x (send evt get-x)]
		 [y (send evt get-y)]
		 [p (xy->grid x y)])
	    (cond
	     [(or (send evt moving?)
		  (send evt entering?)
		  (send evt leaving?))
	      (let ([dc (get-dc)])

		;; update the bars
		(when (and last-p
			   (or (send evt leaving?)
			       (not p)
			       (not (= (cdr p) (cdr last-p)))))
		  (let ([last-row (cdr last-p)])
		    (send dc set-pen WHITE-PEN)
		    (send dc set-brush WHITE-BRUSH)
		    (draw-row-label last-row (list-ref row-numbers last-row))))
		(when (and p
			   (or (send evt entering?)
			       (not last-p)
			       (not (= (cdr last-p) (cdr p)))))
		  (let ([row (cdr p)])
		    (send dc set-pen BAR-PEN)
		    (send dc set-brush BAR-BRUSH)
		    (draw-row-label row (list-ref row-numbers row))))

		(when (and last-p
			   (or (send evt leaving?)
			       (not p)
			       (not (= (car p) (car last-p)))))
		  (let ([last-col (car last-p)])
		    (send dc set-pen WHITE-PEN)
		    (send dc set-brush WHITE-BRUSH)
		    (draw-col-label last-col (list-ref col-numbers last-col))))
		(when (and p
			   (or (send evt entering?)
			       (not last-p)
			       (not (= (car last-p) (car p)))))
		  (let ([col (car p)])
		    (send dc set-pen BAR-PEN)
		    (send dc set-brush BAR-BRUSH)
		    (draw-col-label col (list-ref col-numbers col))))

		(set! last-p p)


		;; update the coordinates
		(send dc set-pen WHITE-PEN)
		(send dc set-brush WHITE-BRUSH)
		(send dc draw-rectangle 0 0 row-label-width col-label-height)
		(when (and (not (send evt leaving?))
			   p)
		  (let* ([i (car p)]
			 [j (cdr p)]
			 [string (loc->string (+ i 1) (+ j 1))]
			 [width (get-string-width string)]
			 [height (get-string-height string)]
			 [sx (- (/ row-label-width 2)
				(/ width 2))]
			 [sy (- (/ col-label-height 2)
				(/ height 2))])
		    (send dc draw-text string sx sy))))]
	     [(send evt button-up?)
	      (when p
		(let* ([i (car p)]
		       [j (cdr p)]
		       [prev (vector-ref (vector-ref grid i) j)]
		       [new
			(cond
			 [(eq? prev UNKNOWN-BRUSH)
			  (if (or (send evt get-alt-down)
				  (send evt get-control-down)
				  (send evt get-meta-down)
				  (send evt get-shift-down))
			      OFF-BRUSH
			      ON-BRUSH)]
			 [(eq? prev ON-BRUSH) UNKNOWN-BRUSH]
			 [(eq? prev OFF-BRUSH) UNKNOWN-BRUSH]
			 [(eq? prev WRONG-BRUSH) UNKNOWN-BRUSH]
			 [else
			  (error 'internal-error
				 "unkown brush in board ~s~n"
				 (vector-ref (vector-ref grid i) j))])])
		  (set! undo-history (cons (make-do i j prev new) undo-history))
		  (set! redo-history null)
		  (vector-set! (vector-ref grid i) j new)
		  (paint-rect i j)))])))]
       [on-paint
	(lambda ()
	  (let ([dc (get-dc)])
	    (send dc clear)
	    (let-values ([(width height) (get-client-size)])

	      (send dc set-pen LINES/NUMBERS-PEN)
	      (let loop ([i grid-x-size])
		(cond
		 [(zero? i) (void)]
		 [else (let loop ([j grid-y-size])
			 (cond
			  [(zero? j) (void)]
			  [else (paint-rect (- i 1) (- j 1))
				(loop (- j 1))]))
		       (loop (- i 1))]))

	      (let loop ([l col-numbers]
			 [n 0])
		(cond
		 [(null? l) (void)]
		 [else
		  (if (and last-p
			   (= (car last-p) n))
		      (begin
			(send dc set-pen BAR-PEN)
			(send dc set-brush BAR-BRUSH))
		      (begin
			(send dc set-pen WHITE-PEN)
			(send dc set-brush WHITE-BRUSH)))

		  (draw-col-label n (car l))
		  (loop (cdr l) (+ n 1))]))

	      (let loop ([l row-numbers]
			 [n 0])
		(cond
		 [(null? l) (void)]
		 [else
		  (if (and last-p
			   (= (cdr last-p) n))
		      (begin
			(send dc set-pen BAR-PEN)
			(send dc set-brush BAR-BRUSH))
		      (begin
			(send dc set-pen WHITE-PEN)
			(send dc set-brush WHITE-BRUSH)))
		  (draw-row-label n (car l))
		  (loop (cdr l)
			(+ n 1))]))
	      
	      (void))))])
      
      (inherit min-width min-height)
      (sequence
	(super-init parent)
	
	(let* ([dc (get-dc)])
	  (set! row-label-width
		(max (get-string-width (loc->string grid-x-size grid-y-size))
		     (apply max (map (lambda (x) (+ x-margin
						    (get-string-width (get-row-label-string x))
						    x-margin))
				     row-numbers))))

	  (let-values ([(width height descent ascent) (send dc get-text-extent "0123456789")])
	    (set! row-label-height (+ y-margin height y-margin)))

	  (set! col-label-height
		(max
		 (get-string-height/descent (loc->string grid-x-size grid-y-size))
		 (apply max
			(map (lambda (l)
			       (let* ([strs (get-col-label-strings l)]
				      [margins (* (length strs) y-margin)]
				      [height (apply + (map get-string-height strs))])
				 (+ margins height)))
			     col-numbers))))

	  (set! col-label-width
		(apply max
		       (map (lambda (l)
			      (let ([label-strings (get-col-label-strings l)])
				(if (null? label-strings)
				    (+ x-margin x-margin) ;; Minimum column label width (no labels)
				    (apply max
					   (map (lambda (x) (+ x-margin
							       (get-string-width x)
							       x-margin))
						label-strings)))))
			    col-numbers))))
	
	(min-width (inexact->exact (+ row-label-width (* grid-x-size col-label-width))))
	(min-height (inexact->exact (+ col-label-height (* grid-y-size row-label-height))))))))