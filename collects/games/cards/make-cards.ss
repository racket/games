(unit/sig cards:make-cards^
  (import [mred : mred^]
	  [card-class : cards:card-class^])

  (define (get-bitmap file)
    (make-object mred:bitmap% file 'gif))
  (define (get-bitmap/dc file)
    (let ([bm (get-bitmap file)])
      (let ([m (make-object mred:memory-dc%)])
	(send m select-object bm)
	m)))

  (define (make-semi dc w h)
    (let* ([bm (make-object mred:bitmap% (floor (/ w 2)) h)]
	   [mdc (make-object mred:memory-dc%)])
      (send mdc select-object bm)
      (let loop ([i (floor (/ w 2))])
	(unless (zero? i)
	  (send mdc blit i 0 1 h dc (* 2 i) 0)
	  (loop (sub1 i))))
      mdc))

  (define tmpframe
    (let* ([f (make-object mred:frame% "Please Wait")])
      (make-object mred:message% "Loading cards..." f)
      (send f stretchable-height #f)
      (send f stretchable-width #f)
      (send f center 'both)
      (send f show #t)
      f))
  (mred:flush-display)
  (mred:yield)

  (define here 
    (let ([cp (collection-path "games" "cards")])
      (lambda (file)
	(build-path cp file))))

  (define back (get-bitmap (here "back.gif")))

  (define deck-of-cards
    (let* ([back (get-bitmap (here "back.gif"))]
	   [w (send back get-width)]
	   [h (send back get-height)]
	   [back (let ([m (make-object mred:memory-dc%)])
		   (send m select-object back)
		   m)]
	   [semi-back (make-semi back w h)])
      (let sloop ([suit 4])
	(if (zero? suit)
	    null
	    (let vloop ([value 13])
	      (sleep)
	      (if (zero? value)
		  (sloop (sub1 suit))
		  (let ([front (get-bitmap/dc
				(here 
				 (format "card-~a-~a.gif"
					 (sub1 value)
					 (sub1 suit))))])
		    (cons (make-object card-class:card%
			    suit
			    value
			    w h
			    front back
			    (make-semi front w h) semi-back)
			  (vloop (sub1 value))))))))))
  
  (send tmpframe show #f))