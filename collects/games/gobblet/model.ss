(module model mzscheme
  (require "sig.ss"
	   (lib "unitsig.ss"))

  (provide model-unit)
  
  (define model-unit
    (unit/sig model^
      (import config^)

      (define SIZES (if (= BOARD-SIZE 3)
			'(1 2 3)
			'(1 2 3 4)))

      ;; A piece is
      ;;  (make-piece num sym)
      ;;  where the sym is 'red or 'yellow
      (define-struct piece (size color))  

      (define red-pieces (map (lambda (sz) (make-piece sz 'red)) SIZES))
      (define yellow-pieces (map (lambda (sz) (make-piece sz 'yellow)) SIZES))
      
      ;; A board is a 
      ;;  (vector (vector (list piece ...) ...) ...)

      (define empty-board
	(make-vector BOARD-SIZE (make-vector BOARD-SIZE null)))

      ;; board-ref : board num num -> piece
      (define (board-ref a i j)
	(vector-ref (vector-ref a i) j))

      ;; board-set : board num num piece -> board
      (define (board-set a i j p)
	;; We implement functional update by copying two vectors
	;;  and modifying them.
	(let ([a2 (copy-vector a)]
	      [r2 (copy-vector (vector-ref a i))])
	  (vector-set! a2 i r2)
	  (vector-set! r2 j p)
	  a2))
      
      ;; copy-vector : vector -> vector
      (define (copy-vector v)
	(list->vector (vector->list v)))

      ;; Utilities ------------------------------

      ;; fold-rowcol : (num alpha -> alpha) alpha -> alpha
      (define (fold-rowcol f v)
	(let iloop ([i 0][v v])
	  (if (= i BOARD-SIZE)
	      v
	      (iloop (add1 i) (f i v)))))

      ;; fold-board : (num num alpha -> alpha) alpha -> alpha
      (define (fold-board f v)
	(fold-rowcol (lambda (i v)
		       (fold-rowcol (lambda (j v)
				      (f i j v))
				    v))
		     v))

      ;; other : sym -> sym
      (define (other c)
	(if (eq? c 'red) 'yellow 'red))
      
      ;; Model ------------------------------

      ;; move : board piece num-or-#f num-or-#f num num (board -> alpha) (-> alpha)
      ;;        -> alpha
      ;; Given a board, piece, current location of the piece (or #f if
      ;; not on the board), and target location for the piece, either
      ;; allows the move and calls the continuation k with the new
      ;; board, or disallows and calls the failure continuation.
      (define (move board p from-i from-j to-i to-j k fail-k)
	(let ([pl (board-ref board to-i to-j)])
	  ;; The move is allowed if the target space is empty or the
	  ;;  top piece is smaller than this one:
	  (if (or (null? pl)
		  (< (piece-size (car pl)) (piece-size p)))
	      ;; Allowed:
	      (let ([new-board (if from-i
				   ;; Remove the piece from the old spot:
				   (board-set board from-i from-j 
					      (cdr (board-ref board from-i from-j)))
				   ;; Wasn't on the board, yet:
				   board)])
		;; Add the piece at its new spot, and continue
		(k (board-set new-board to-i to-j (cons p pl))))
	      ;; Not allowed; fail
	      (fail-k))))

      ;; winner? : board sym -> bool
      ;;  Checks whether the given color has BOARD-SIZE in a row
      (define (winner? board c)
	(let ([row/col
	       (lambda (board-ref)
		 (fold-rowcol (lambda (i v)
				(or v
				    (fold-rowcol (lambda (j v)
						   (and v
							(let ([pl (board-ref board i j)])
							  (and (pair? pl)
							       (eq? c (piece-color (car pl)))))))
						 #t)))
			      #f))]
	      [diag
	       (lambda (flip)
		 (fold-rowcol (lambda (i v)
				(and v
				     (let ([pl (board-ref board i (flip i))])
				       (and (pair? pl)
					    (eq? c (piece-color (car pl)))))))
			      #t))])
	  (or (row/col board-ref)
	      (row/col (lambda (a i j) (board-ref a j i)))
	      (diag (lambda (x) x))
	      (diag (lambda (x) (- BOARD-SIZE 1 x)))))))))
