(module heuristics mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "sig.ss")

  (provide 3x3-simple-heuristics-unit
	   4x4-simple-heuristics-unit)

  (define 3x3-simple-heuristics-unit
    (unit/sig heuristic^
      (import config^ model^)
  
      (define rate-board
	(lambda (board me to-i to-j)
	  (+ (let ([l (board-ref board 1 1)])
	       (if (pair? l)
		   (if (eq? (piece-color (car l)) me)
		       2
		       -2)
		   0))
	     (random))))))

  (define 4x4-simple-heuristics-unit
    (unit/sig heuristic^
      (import config^ model^)
  
      (define rate-board
	(lambda (board me to-i to-j)
	  (+ (random) (if (and (top-color? board to-i to-j (other me))
			       (3-in-a-row? board to-i to-j (other me)))
			  -2
			  0))))

      (define (top-color? board i j c)
	(let ([l (board-ref board i j)])
	  (and (pair? l)
	       (eq? (piece-color (car l)) c))))))

  )

