(module model mzscheme
  (require "sig.ss"
	   (lib "unitsig.ss"))

  (provide model-unit)
  
  (define model-unit
    (unit/sig model^
      (import config^)

      (define JR? (= BOARD-SIZE 3))
      (define SIZES (if (= BOARD-SIZE 3)
			'(1 2 3)
			'(1 2 3 4)))
      (define PIECE-COUNT (sub1 BOARD-SIZE))
      
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
		  (and (< (piece-size (car pl)) (piece-size p))
		       (or from-i
			   JR?
			   ;; In 4x4 game, can't move onto board on top
			   ;;  of piece unless it's part of 3-in-a-row
			   (and (not (eq? (piece-color (car pl)) (piece-color p)))
				(3-in-a-row? board to-i to-j (piece-color (car pl)))))))
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

      ;; 3-in-a-row? : board num num color -> bool
      (define (3-in-a-row? board i j c)
	(or (n-in-a-row/col? 3 board i j c)
	    (and (= i j)
		 (n-in-a-diag? 3 board c backslash-diag-flip))
	    (and (= i (- BOARD-SIZE 1 j))
		 (n-in-a-diag? 3 board c slash-diag-flip))))

      ;; n-in-a-row/col? : num board num num color -> bool
      (define (n-in-a-row/col? n board i j c)
	(let ([row/col?
	       (lambda (board-ref)
		 (= n
		    (fold-rowcol (lambda (z v)
				   (+ v
				      (let ([pl (board-ref z)])
					(if (and (pair? pl)
						 (eq? c (piece-color (car pl))))
					    1
					    0))))
				 0)))])
	  (or (row/col? (lambda (z) (board-ref board i z)))
	      (row/col? (lambda (z) (board-ref board z j))))))

      ;; n-in-a-diag? : num board color (num -> num) -> bool
      (define (n-in-a-diag? n board c flip)
	(= n
	   (fold-rowcol (lambda (i v)
			  (+ v
			     (let ([pl (board-ref board i (flip i))])
			       (if (and (pair? pl)
					(eq? c (piece-color (car pl))))
				   1
				   0))))
			0)))
      (define backslash-diag-flip (lambda (x) x))
      (define slash-diag-flip (lambda (x) (- BOARD-SIZE 1 x)))

      ;; winner? : board sym -> bool
      ;;  Checks whether the given color has BOARD-SIZE in a row
      (define (winner? board c)
	(or (n-in-a-diag? BOARD-SIZE board c backslash-diag-flip)
	    (n-in-a-diag? BOARD-SIZE board c slash-diag-flip)
	    (fold-rowcol (lambda (i v)
			   (or v
			       (n-in-a-row/col? BOARD-SIZE board i i c)))
			 #f))))))