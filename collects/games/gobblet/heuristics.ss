(module heuristics mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "sig.ss"
	   "plays-3x3.ss")

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
      
      (define (make-3x3-canned-moves canonicalize init-memory) 
	(for-each (lambda (play)
		    (let ([key+xform (canonicalize (car play) #f)])
		      (hash-table-put! init-memory
				       (car key+xform)
				       (let-values ([(from-i from-j)
						     (if (list-ref play 2)
							 (unapply-xform (cdr key+xform) (list-ref play 2))
							 (values #f #f))]
						    [(to-i to-j)
						     (unapply-xform (cdr key+xform) (list-ref play 3))])
					 (list
					  (cons +inf.0
						(make-plan
						 (list-ref play 1)
						 from-i from-j to-i to-j
						 (cdr key+xform)
						 (list-ref play 4))))))))
		  3x3-plays)
	(lambda (board me k xform)
	  null))

      (define (make-3x3-no-canned-moves canonicalize init-memory) 
	(lambda (board me k xform)
	  null))

      (define (make-3x3-rate-board canon)
	(lambda (board me to-i to-j)
	  (+ (rate-cell board me 1 1)
	     (random))))

      (define (rate-cell board me i j)
	(let ([l (board-ref board i j)])
	  (if (pair? l)
	      (if (eq? (piece-color (car l)) me)
		  2
		  -2)
	      0)))

      (define (make-4x4-canned-moves canon init-memory) 
	(lambda (board me k xform)
	  null))

      (define (make-4x4-rate-board canon)
	(lambda (board me to-i to-j)
	  (+ (random) 
	     (if (and (top-color? board to-i to-j (other me))
		      (3-in-a-row? board to-i to-j (other me)))
		 -10
		 0)
	     ;; Controlling the middle cells seems good:
	     (rate-cell board me 1 1)
	     (rate-cell board me 1 2)
	     (rate-cell board me 2 1)
	     (rate-cell board me 2 2))))
      
      (define (top-color? board i j c)
	(let ([l (board-ref board i j)])
	  (and (pair? l)
	       (eq? (piece-color (car l)) c)))))))
