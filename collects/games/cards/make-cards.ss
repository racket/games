
(module make-cards mzscheme
  (require (lib "class.ss")
	   (prefix mred: (lib "mred.ss" "mred"))
	   (prefix card-class: "card-class.ss"))
  
  (provide back deck-of-cards make-card)

  (define (get-bitmap file)
    (make-object mred:bitmap% file))

  (define (make-semi bm-in w h)
    (let* ([bm (make-object mred:bitmap% (floor (/ w 2)) h)]
	   [mdc (make-object mred:bitmap-dc%)])
      (send mdc set-bitmap bm)
      (let loop ([i (floor (/ w 2))])
	(unless (zero? i)
	  (send mdc draw-bitmap-section bm-in i 0 (* 2 i) 0 1 h)
	  (loop (sub1 i))))
      (send mdc set-bitmap #f)
      bm))

  (define here 
    (let ([cp (collection-path "games" "cards")])
      (lambda (file)
	(build-path cp 
		    (if ((mred:get-display-depth)  . <= . 8)
			"locolor"
			"hicolor")
		    file))))

  (define back (get-bitmap (here "card-back.png")))

  (define semi-back 
    (let ([w (send back get-width)]
	  [h (send back get-height)])
      (make-semi back w h)))

  (define deck-of-cards
    (let* ([w (send back get-width)]
	   [h (send back get-height)])
      (let sloop ([suit 4])
	(if (zero? suit)
	    null
	    (let vloop ([value 13])
	      (sleep)
	      (if (zero? value)
		  (sloop (sub1 suit))
		  (let ([front (get-bitmap
				(here 
				 (format "card-~a-~a.png"
					 (sub1 value)
					 (sub1 suit))))])
		    (cons (make-object card-class:card%
			    suit
			    value
			    w h
			    front back
			    (make-semi front w h) semi-back)
			  (vloop (sub1 value))))))))))
  
  (define (make-card front-bm back-bm suit-id value)
    (let ([w (send back get-width)]
	  [h (send back get-height)])
      (make-object card-class:card%
		   suit-id
		   value
		   w h
		   front-bm (or back-bm back)
		   (make-semi front-bm w h) 
		   (if back-bm 
		       (make-semi back-bm w h)
		       semi-back)))))