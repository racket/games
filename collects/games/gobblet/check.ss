(module check mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "sig.ss"
	   "model.ss"
	   "explore.ss"
	   "heuristics.ss")

  (define board-size 3)
  (define cannon-size +inf.0)

  (invoke-unit/sig
   (compound-unit/sig
    (import)
    (link
     [CONFIG : config^ ((unit/sig config^
			  (import)
			  (define BOARD-SIZE board-size)))]
     [MODEL : model^ (model-unit CONFIG)]
     [HEURISTICS : heuristics^ (heuristics-unit CONFIG MODEL EXPLORE)]
     [EXPLORE : explore^ (explore-unit CONFIG MODEL)]
     [ROBOT : () ((unit/sig ()
		    (import config^ explore^ model^ heuristics^)
		    
		    (define (mk-search)
		      (make-search make-3x3-rate-board make-3x3-canned-moves))
		    
		    (let ([search (mk-search)]
			  [cnt 0]
			  [move-map (make-hash-table 'equal)]
			  [canonicalize (make-canonicalize)])
		      (let loop ([board empty-board]
				 [depth 0]
				 [history null])
			(set! cnt (+ cnt 1))
			(when (= cnt 10)
			  (set! cnt 0)
			  (set! search (mk-search)))
			(printf "------------~n~a~n" (board->string depth board))
			(cond
			 [(winner? board 'red)
			  (void)]
			 [(winner? board 'yellow)
			  (error '! "yellow wins")]
			 [else
			  (let ([key+xform (canonicalize board 'red)])
			    (hash-table-get 
			     move-map (car key+xform)
			     (lambda ()
			       (let ([play (search 300.0 1 2 'red board history)])
				 (hash-table-put! move-map (car key+xform)
						  (list (piece-size (car play))
							(and (list-ref play 1)
							     (apply-xform (cdr key+xform)
									  (list-ref play 1) (list-ref play 2)))
							(apply-xform (cdr key+xform)
								     (list-ref play 3) (list-ref play 4))))
				 (printf "~a ~a ~a~n" (piece-color (car play)) (piece-size (car play)) (cdr play))
				 (let ([new-board (apply-play board play)])
				   (unless (winner? new-board 'red)
				     (let ([pss (available-off-board new-board 'yellow)])
				       (for-each
					(lambda (ps)
					  (fold-board
					   (lambda (i j v)
					     (move new-board (list-ref yellow-pieces (car ps))
						   #f #f i j
						   (lambda (newer-board)
						     (loop newer-board
							   (add1 depth)
							   (list* new-board board history)))
						   void))
					   (void)))
					pss))
				     (fold-board
				      (lambda (from-i from-j v)
					(let ([ps (board-ref new-board from-i from-j)])
					  (when (and (pair? ps)
						     (eq? 'yellow (piece-color (car ps))))
					    (fold-board
					     (lambda (to-i to-j v)
					       (move new-board (car ps)
						     from-i from-j to-i to-j
						     (lambda (newer-board)
						       (loop newer-board
							     (add1 depth)
							     (list* new-board board history)))
						     void))
					     (void)))))
				      (void))))))))]))
		      (hash-table-for-each move-map
					   (lambda (k v)
					     (printf "~s~n" (cons k v))))))
		  CONFIG EXPLORE MODEL HEURISTICS)])
    (export))))
