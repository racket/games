
(define f (make-object mred:frame% null "Cards" -1 -1 550 300))
(define p (make-object mred:vertical-panel% f))
(define c (make-object mred:media-canvas% p
		       -1 -1 -1 -1
		       ""
		       (bitwise-ior wx:const-mcanvas-no-v-scroll
				    wx:const-mcanvas-no-h-scroll)))

(define cards:pasteboard%
  (class mred:pasteboard% ()
    (inherit begin-edit-sequence end-edit-sequence get-admin
	     invalidate-bitmap-cache
	     find-next-selected-snip find-first-snip find-snip
	     set-before set-after
	     add-selected is-selected? no-selected
	     get-snip-location move-to
	     dc-location-to-buffer-location
	     set-selection-visible)
    (rename [super-after-select after-select]
	    [super-on-default-event on-default-event]
	    [super-on-interactive-move on-interactive-move] 
	    [super-interactive-adjust-move interactive-adjust-move]
	    [super-after-interactive-move after-interactive-move])
    (private 
      [selecting? #f]
      [dragging? #f]
      [decided-start? #f]
      [can-shuffle? #f]
      [shuffle-hilite? #f]
      [click-base null]
      [get-snip-bounds
       (lambda (s)
	 (let ([xbox (box 0)]
	       [ybox (box 0)])
	   (get-snip-location s xbox ybox #f)
	   (let ([l (unbox xbox)]
		 [t (unbox ybox)])
	     (get-snip-location s xbox ybox #t)
	     (values l t (unbox xbox) (unbox ybox)))))]
      [make-overlapping-list
       (lambda (s so-far)
	 (let-values ([(sl st sr sb) (get-snip-bounds s)])
	   (let loop ([t (find-first-snip)][so-far so-far])
	     (if (or (null? t) (eq? s t))
		 so-far
		 (let ([l (if (and (not (memq t so-far))
				   (let-values ([(tl tt tr tb) 
						 (get-snip-bounds t)])
				     (and (or (<= sl tl sr)
					      (<= sl tr sr))
					  (or (<= st tt sb)
					      (<= st tb sb)))))
			      (make-overlapping-list t (cons t so-far))
			      so-far)])
		   (loop (send t next) l))))))]
      [get-reverse-selected-list
       (lambda ()
	 (let loop ([s (find-next-selected-snip null)][l null])
	   (if (null? s)
	       l
	       (loop (find-next-selected-snip s) (cons s l)))))]
      [get-shuffle-box
       (lambda ()
	 (let ([xb (box 0)][yb (box 0)]
	       [wb (box 0)][hb (box 0)])
	   (send (get-admin) get-view xb yb wb hb)
	   (let ([sx (* 1/10 (unbox wb))]
		 [sy (* 7/10 (unbox hb))]
		 [sw (* 8/10 (unbox wb))]
		 [sh (* 2/10 (unbox hb))])
	     (values sx sy sw sh))))]
      [shuffle-list 
       (lambda (l c)
	 (if (zero? c)
	     l
	     (let-values ([(a b)
			   (let ([half (floor (/ (length l) 2))])
			     (values
			      (let loop ([l l][n half])
				(if (zero? n)
				    null
				    (cons (car l) (loop (cdr l) (sub1 n)))))
			      (list-tail l half)))])
	       (shuffle-list
		(let loop ([a a][b b][l null])
		  (cond
		    [(null? a) (append (reverse b) l)]
		    [(null? b) (append (reverse a) l)]
		    [(zero? (random 2))
		     (loop (cdr a) b (cons (car a) l))]
		    [else
		     (loop a (cdr b) (cons (car b) l))]))
		(sub1 c)))))]
      [shuffle
       (lambda (selected-list) ; cards to shuffle in back->front order
	 (let* ([permuted-list
		 (shuffle-list selected-list 7)]
		[get-pos
		 (lambda (s)
		   (let ([xb (box 0)]
			 [yb (box 0)])
		     (get-snip-location s xb yb)
		     (cons (unbox xb) (unbox yb))))]
		[sel-loc-list (map get-pos selected-list)]
		[perm-loc-list (map get-pos permuted-list)])
	   (for-each
	    (lambda (s start-pos end-pos)
	      (let* ([sx (car start-pos)]
		     [sy (cdr start-pos)]
		     [ex (car end-pos)]
		     [ey (cdr end-pos)]
		     [steps (max 1 (floor (/ 50 (length selected-list))))])
		(let loop ([i 1])
		  (unless (> i steps)
		    (let ([x (+ sx (* (/ i steps) (- ex sx)))]
			  [y (+ sy (* (/ i steps) (- ey sy)))])
		      (move-to s x y)
		      (wx:flush-display)
		      (loop (add1 i)))))))
	    permuted-list perm-loc-list sel-loc-list)
	   (let loop ([l permuted-list])
	     (unless (null? l)
	       (set-before (car l) null)
	       (loop (cdr l))))
	   (no-selected)))])
    (public
      [on-paint
       (lambda (before? dc l t r b dx dy caret)
	 (when before?
	   (let-values ([(sx sy sw sh)
			 (get-shuffle-box)]
			[(old-b) (send dc get-brush)])
	     (when shuffle-hilite?
	       (send dc set-brush (send wx:the-brush-list
					find-or-create-brush
					"RED" wx:const-solid)))
	     (send dc draw-rectangle (+ dx sx) (+ dy sy) sw sh)
	     (when shuffle-hilite?
	       (send dc set-brush old-b))
	     (let ([xb (box 0)][yb (box 0)])
	       (send dc get-text-extent "Shuffle" xb yb)
	       (send dc draw-text "Shuffle" 
		     (+ dx sx (/ (- sw (unbox xb)) 2))
		     (+ dy sy 10))))))]
      [after-select
       (lambda (s on?)
	 (super-after-select s on?)
	 (unless (or (not on?) selecting?)
	   (set! selecting? #t)
	   (begin-edit-sequence)
	   (let ([l (make-overlapping-list s (list s))])
	     (for-each add-selected l))
	   (let loop ([snip (find-next-selected-snip null)][prev null])
	     (unless (null? snip)
	       (if (null? prev)
		   (set-before snip null)
		   (set-after snip prev))
	       (loop (find-next-selected-snip snip) snip)))
	   (end-edit-sequence)
	   (set! selecting? #f)))]
      [on-interactive-move
       (lambda ()
	 (super-on-interactive-move)
	 (set! decided-start? #f)
	 (set! dragging? #t))]
      [interactive-adjust-move
       (lambda (snip xb yb)
	 (super-interactive-adjust-move snip xb yb)
	 (let-values ([(l t r b) (get-snip-bounds snip)])
	   (let ([wb (box 0)][hb (box 0)])
	     (send (get-admin) get-view null null wb hb)
	     (let ([max-x (- (unbox wb) (- r l))]
		   [max-y (- (unbox hb) (- b t))])
	       (when (> (unbox xb) max-x)
		 (set-box! xb max-x))
	       (when (> (unbox yb) max-y)
		 (set-box! yb max-y))))))]
      [after-interactive-move
       (lambda ()
	 (set! dragging? #f)
	 (super-after-interactive-move)
	 (when shuffle-hilite?
	   (let ([shuffle-cards (get-reverse-selected-list)])
	     ; Do animation via a callback since we're in an
	     ; edit sequence
	     (semaphore-callback
	      (make-semaphore 1)
	      (lambda ()
		(shuffle shuffle-cards)
		(let-values ([(sx sy sw sh) (get-shuffle-box)])
		  (set! shuffle-hilite? #f)
		  (invalidate-bitmap-cache sx sy sw sh)))))))]
      [on-default-event
       (lambda (e)
	 (let-values ([(lx ly) 
		       (dc-location-to-buffer-location (send e get-x) 
						       (send e get-y))])
	   ; Clicking on a "selected" card unselects others
	   ; in this interface
	   (when (send e button-down?)
	     (let ([s (find-snip lx ly)])
	       (unless (or (null? click-base) (null? s) (eq? s click-base))
		 (no-selected))
	       (set! click-base s)))
	   (when (and dragging? (or (not decided-start?) can-shuffle?))
	     (let-values ([(sx sy sw sh) (get-shuffle-box)])
	       (let ([in? (and (<= sx lx (+ sx sw))
			       (<= sy ly (+ sy sh)))])
		 (unless decided-start?
		   (set! decided-start? #t)
		   (set! can-shuffle? (not in?)))
		 (when (and (not (eq? in? shuffle-hilite?))
			    can-shuffle?)
		   (set! shuffle-hilite? in?)
		   (invalidate-bitmap-cache sx sy sw sh))))))
	 (super-on-default-event e))]
      [on-double-click
       (lambda (s e)
	 (begin-edit-sequence)
	 (let ([l (get-reverse-selected-list)])
	   (for-each (lambda (s) (send s flip)) l)
	   (let loop ([l (reverse! l)])
	     (unless (null? l)
	       (set-before (car l) null)
	       (loop (cdr l)))))
	 (no-selected)
	 (end-edit-sequence))])
    (sequence
      (super-init)
      (set-selection-visible #f))))

(define pb (make-object cards:pasteboard%))
(send c set-media pb)

(define card:image-snip%
  (class wx:image-snip% (front-file back-file)
    (inherit set-bitmap)
    (private
      [flipped? #f]
      [front-bitmap (make-object wx:bitmap% (normalize-path front-file)
				 wx:const-bitmap-type-gif)]
      [back-bitmap (make-object wx:bitmap% (normalize-path back-file)
				wx:const-bitmap-type-gif)])
    (public
      [is-flipped? (lambda () flipped?)]
      [flip
       (lambda ()
	 (set! flipped? (not flipped?))
	 (set-bitmap (if flipped? 
			 back-bitmap
			 front-bitmap)))]
      [resize
       (lambda (w h) (void))])
    (sequence
      (super-init null 0 #f #f)
      (flip))))

(printf "Loading cards~n")

(define cards
  (let sloop ([suite 4])
    (if (zero? suite)
	null
	(let vloop ([value 13])
	  (sleep)
	  (if (zero? value)
	      (sloop (sub1 suite))
	      (cons (make-object card:image-snip%
				 (format "card-~a-~a.gif"
					 (sub1 value)
					 (sub1 suite))
				 "back.gif")
		    (vloop (sub1 value))))))))

(send f show #t)
(wx:flush-display)
(wx:yield)

(let* ([margin 10]
       [card-width 100]
       [delta (/ (- (send c get-width) (* 2 margin) card-width) 
		 (length cards))])
  (let loop ([l cards][over null][x-pos margin])
    (unless (null? l)
      (send pb insert (car l) over x-pos margin)
      (loop (cdr l) (car l) (+ x-pos delta)))))
