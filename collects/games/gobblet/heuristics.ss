(module heuristics mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "sig.ss")

  (provide heuristics-unit)

  (define heuristics-unit
    (unit/sig heuristics^
      (import config^ model^ explore^)

      (define (gen-board seq)
	(let loop ([board empty-board][seq seq][who 'red])
	  (if (null? seq)
	      board
	      (let ([m (car seq)])
		(move board 
		      (list-ref (if (eq? who 'red) red-pieces yellow-pieces)
				(list-ref m 0))
		      (list-ref m 1)
		      (list-ref m 2)
		      (list-ref m 3)
		      (list-ref m 4)
		      (lambda (b) (loop b (cdr seq) (other who)))
		      void)))))

      (define (canon=? k k2+xform2)
	(if (equal? k (car k2+xform2))
	    (cdr k2+xform2)
	    #f))

      (define GOOD 1000.0)
      (define BAD -1000.0)
      
      (define (make-3x3-canned-moves canon) 
	(let ([start (canon empty-board 'red)]
	      [start-yellow (canon (gen-board '((2 #f #f 1 1)))
				   'yellow)]
	      [red-2nd (canon (gen-board '((2 #f #f 1 1)
					   (2 #f #f 2 2)))
			      'red)]
	      [yellow-2nd (canon (gen-board '((2 #f #f 1 1)
					      (2 #f #f 2 2)
					      (2 #f #f 1 2)))
				 'yellow)]
	      [red-win (canon (gen-board '((2 #f #f 1 1)
					   (2 #f #f 2 2)
					   (2 #f #f 1 2)
					   (2 #f #f 1 0)))
			      'red)])
	  (lambda (board me k xform)
	    (cond
	     [(canon=? k start)
	      ;; Empty board
	      ;; Red start in middle
	      (list (cons GOOD
			  (make-plan (list-ref red-pieces 2) #f #f 1 1 xform)))]
	     [(canon=? k start-yellow)
	      ;; Red moves on center
	      ;; Yellow start on corner
	      (list (cons GOOD
			  (make-plan (list-ref yellow-pieces 2) #f #f 2 2 xform)))]
	     [(canon=? k red-2nd)
	      => (lambda (xform2)
		   ;; Set up for yellow mistake...
		   (list (cons GOOD
			       (make-plan (list-ref red-pieces 2) #f #f 1 2 xform2))))]
	     [(canon=? k yellow-2nd)
	      => (lambda (xform2)
		   ;; Don't make the mistake
		   (list (cons BAD
			       (make-plan (list-ref yellow-pieces 2) #f #f 1 0 xform2))
			 (cons BAD
			       (make-plan (list-ref yellow-pieces 2) 2 2 1 0 xform2))))]
	     [(canon=? k red-win)
	      => (lambda (xform2)
		   ;; Yellow made a mistake; go for the win!
		   (list (cons GOOD
			       (make-plan (list-ref red-pieces 1) #f #f 0 2 xform2))))]
	     [else null]))))

      (define (make-3x3-rate-board canon)
	(lambda (board me to-i to-j)
	  (+ (let ([l (board-ref board 1 1)])
	       (if (pair? l)
		   (if (eq? (piece-color (car l)) me)
		       2
		       -2)
		   0))
	     (random))))

      (define (make-4x4-canned-moves canon) 
	(lambda (board me k xform)
	  null))

      (define (make-4x4-rate-board canon)
	(lambda (board me to-i to-j)
	  (+ (random) (if (and (top-color? board to-i to-j (other me))
			       (3-in-a-row? board to-i to-j (other me)))
			  -2
			  0))))
      
      (define (top-color? board i j c)
	(let ([l (board-ref board i j)])
	  (and (pair? l)
	       (eq? (piece-color (car l)) c)))))))
