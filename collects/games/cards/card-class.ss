(unit/sig cards:card-class^
  (import [mred : mred^]
	  [snipclass : cards:snipclass^])

  (define card%
    (class mred:snip% (suit-id value width height front back semi-front semi-back)
	   (inherit set-snipclass set-count get-admin)
	   (private
	    [flipped? #f]
	    [semi-flipped? #f]
	    [can-flip? #t]
	    [can-move? #t]
	    [snap-back? #f]
	    [refresh
	     (lambda ()
	       (let ([a (get-admin)])
		 (when a
		   (send a needs-update this 0 0 width height))))])
	   (public
	    [face-down? (lambda () flipped?)]
	    [flip
	     (lambda ()
	       (set! flipped? (not flipped?))
	       (refresh))]
	    [semi-flip
	     (lambda ()
	       (set! semi-flipped? (not semi-flipped?))
	       (refresh))]
	    [face-up (lambda () (when flipped? (flip)))]
	    [face-down (lambda () (unless flipped? (flip)))]
	    [get-suit-id
	     (lambda () suit-id)]
	    [get-suit
	     (lambda ()
	       (case suit-id
		 [(1) 'clubs]
		 [(2) 'diamonds]
		 [(3) 'hearts]
		 [(4) 'spades]))]
	    [get-value
	     (lambda () value)]
	    [user-can-flip
	     (case-lambda
	      [() can-flip?]
	      [(f) (set! can-flip? (and f #t))])]
	    [user-can-move
	     (case-lambda
	      [() can-move?]
	      [(f) (set! can-move? (and f #t))])]
	    [snap-back-after-move
	     (case-lambda
	      [() snap-back?]
	      [(f) (set! snap-back? (and f #t))])]
	    [card-width (lambda () width)]
	    [card-height (lambda () height)])
	   (override
	     [resize
	      (lambda (w h) (void))]
	    [get-extent
	     (lambda (dc x y w h descent space lspace rspace)
	       (map
		(lambda (b)
		  (when b
		    (set-box! b 0)))
		(list descent space lspace rspace))
	       (when w (set-box! w width))
	       (when h (set-box! h height)))]
	    [draw
	     (lambda (dc x y left top right bottom dx dy draw-caret)
	       (if semi-flipped?
		   (send dc draw-bitmap (if flipped? semi-back semi-front) (+ x (/ width 4)) y)
		   (send dc draw-bitmap (if flipped? back front) x y)))]
	    [copy (lambda () (make-object card% suit-id value width height 
					  front back semi-front semi-back))])
	   (private
	     [save-x (box 0)]
	     [save-y (box 0)])
	   (public
	     [remember-location
	      (lambda (pb)
		(send pb get-snip-location this save-x save-y))]
	     [back-to-original-location
	      (lambda (pb)
		(when snap-back?
		  (send pb move-to this (unbox save-x) (unbox save-y))))])
	   (sequence
	     (super-init)
	     (set-count 1)
	     (set-snipclass snipclass:sc)
	     (flip)))))

