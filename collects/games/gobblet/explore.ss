(module explore mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss")
	   "sig.ss"
	   "model.ss")

  (define BOARD-SIZE 3)

  (define-values/invoke-unit/sig model^
    model-unit #f config^)

  (define (max/me a b)
    (if (car a)
	(if (car b)
	    (if ((car a) . >= . (car b))
		a
		b)
	    a)
	b))

  (define (max/him a b t)
    (if (car a)
	(if (car b)
	    (if ((car a) . > . (- (car b)))
		a
		(cons (- (car b)) t))
	    a)
	(cons (- (car b)) t)))

  (define (stack->string s)
    (let ([s (apply string-append 
		    "...."
		    (map (lambda (p)
			   (list-ref (if (eq? 'red (piece-color p))
					 '("_" "i" "I")
					 '("=" "o" "O"))
				     (piece-size p)))
			 s))])
      (substring s (- (string-length s) BOARD-SIZE))))
				  
  (define (board->string depth b)
    (let jloop ([j 0])
      (if (= j 3)
	  ""
	  (string-append
	   (make-string depth #\space)
	   (let iloop ([i 0])
	     (if (= i 3)
		 ""
		 (string-append (stack->string (board-ref b i j))
				" "
				(iloop (add1 i)))))
	   "\n"
	   (jloop (add1 j))))))

  (define (rate-board board me)
    (random)
    ;; BAD HEURISTIC?
    ;; Count number of my pieces versus other pieces
    #;
    (fold-board (lambda (i j v)
		  (+ v
		     (let ([l (board-ref board i j)])
		       (if (pair? l)
			   (if (eq? (piece-color (car l)) me)
			       1
			       -1)
			   0))))
		;; Add randomness, less than non-randomness:
		(random)))

  (define hit 0)
  (define explore 0)

  ;; -> (cons val move-thunk)
  (define (minmax depth max-depth memory canonicalize me him my-pieces his-pieces board)
    (break-enabled #t)
    (break-enabled #f)
    (set! hit (add1 hit))
    (let* ([board-key+xform (canonicalize board me)]
	   [board-key (car board-key+xform)]
	   [xform (cdr board-key+xform)]
	   [key (cons board-key (- max-depth depth))]
	   [win/lose-key board-key])
      (cond
       ;; Check for known win at arbitrary depth:
       [(hash-table-get memory win/lose-key (lambda () #f)) => (lambda (x) (values x xform))]
       ;; Check for known value at specific max depth:
       [(hash-table-get memory key (lambda () #f)) => (lambda (x) (values x xform))]
       [(winner? board him)
	(hash-table-put! memory win/lose-key '(-inf.0))
	(values '(-inf.0) xform)]
       [(winner? board me)
	(hash-table-put! memory win/lose-key '(+inf.0))
	(values '(+inf.0) xform)]
       [(depth . >= . max-depth)
	(let ([l (list (rate-board board me))])
	  (hash-table-put! memory key l)
	  (values l xform))]
       [else 
	(set! explore (add1 explore))
	;; In case we get back here while we're looking, claim an unknown tie:
	(hash-table-put! memory win/lose-key `(0 loop! ,me ,depth ,max-depth))
	(with-handlers ([exn:break? (lambda (x)
				      (hash-table-remove! memory win/lose-key)
				      (raise x))])
	  (let ([add-piece-goodness
		 (lambda (max-depth)
		   (let loop ([avail-pieces my-pieces]
			      [keep-pieces null]
			      [played-sizes null])
		     (cond
		      [(null? avail-pieces) '(#f)]
		      [(memq (caar avail-pieces) played-sizes)
		       (loop (cdr avail-pieces)
			     (cons (car avail-pieces) keep-pieces)
			     played-sizes)]
		      [else
		       (max/me
			;; Try other pieces:
			(loop (cdr avail-pieces)
			      (cons (car avail-pieces) keep-pieces)
			      (cons (caar avail-pieces) played-sizes))
			;; Try this one:
			(let ([p (list-ref (if (eq? me 'red) red-pieces yellow-pieces) 
					   (caar avail-pieces))]
			      [my-pieces (append 
					  keep-pieces
					  (if (null? (cdar avail-pieces))
					      null
					      (list (cdar avail-pieces)))
					  (cdr avail-pieces))])
			  (fold-board
			   (lambda (i j v)
			     (move board p #f #f i j
				   (lambda (new-board)
				     (let-values ([(sv sxform)
						   (minmax (add1 depth) max-depth memory canonicalize
							   him me his-pieces my-pieces new-board)])
				       (max/him v
						sv
						(vector p #f #f i j xform))))
				   (lambda () v)))
			   '(#f))))])))]
		[move-piece-goodness
		 (lambda (max-depth)
		   (fold-board
		    (lambda (from-i from-j v)
		      (let ([l (board-ref board from-i from-j)])
			(if (and (pair? l)
				 (eq? me (piece-color (car l))))
			    (fold-board
			     (lambda (to-i to-j v)
			       (move board (car l) from-i from-j to-i to-j
				     (lambda (new-board)
				       (let-values ([(sv sxform)
						     (minmax (add1 depth) max-depth memory canonicalize
							     him me his-pieces my-pieces new-board)])
					 
					 (max/him v 
						  sv
						  (vector (car l) from-i from-j to-i to-j xform))))
				     (lambda ()
				       v)))
			     v)
			    v)))
		    '(#f)))])
	    (let ([goodness (let ([v (max/me (add-piece-goodness max-depth) 
					     (move-piece-goodness max-depth))])
			      (if (car v)
				  v
				  '(-inf.0)))])
	      (hash-table-remove! memory win/lose-key)
	      (let ([key (if (and ((car goodness) . < . +inf.0)
				  ((car goodness) . > . -inf.0))
			     ;; Result is only valid to current depth limit:
			     key
			     ;; Result is valid to any depth:
			     win/lose-key)])
		(hash-table-put! memory key goodness))
	      (values goodness xform))))])))

  (define (make-search)
    (let ([memory (make-hash-table 'equal)]
	  [canonicalize (make-canonicalize +inf.0)])
      (lambda (timeout depth-limit me board)
	(let ([result #f]
	      [once-sema (make-semaphore)]
	      [result-sema (make-semaphore)])
	  (let ([t (thread
		    (lambda ()
		      (break-enabled #f)
		      (with-handlers ([exn:break? void])
			(let loop ([limit 1])
			  (set! hit 0)
			  (set! explore 0)
			  (set! result
				(let-values ([(v xform)
					      (minmax 0
						      limit
						      memory
						      canonicalize
						      me
						      (other me)
						      (available-off-board board me)
						      (available-off-board board (other me))
						      board)])
				  (let ([x (cdr v)])
				    (let-values ([(from-i from-j)
						  (if (vector-ref x 1)
						      (unapply-xform xform (apply-xform (vector-ref x 5)
											(vector-ref x 1)
											(vector-ref x 2)))
						      (values #f #f))]
						 [(to-i to-j)
						  (unapply-xform xform (apply-xform (vector-ref x 5)
										    (vector-ref x 3)
										    (vector-ref x 4)))])
				      (cons (car v)
					    (lambda ()
					      (list (vector-ref x 0) from-i from-j to-i to-j)))))))
			  (printf "~a -> ~a [~a] {~a, ~a}~n" 
				  limit 
				  ((cdr result))
				  (car result)
				  hit explore)
			  (semaphore-post once-sema)
			  (if (and ((car result) . < . +inf.0)
				   ((car result) . > . -inf.0)
				   (limit . < . depth-limit))
			      (loop (add1 limit))
			      (begin
				(set! hit 0)
				(set! explore 0)
				(semaphore-post result-sema)))))))])
	    (sync/timeout timeout result-sema)
	    (semaphore-wait once-sema)
	    (break-thread t)
	    (sync t)
	    #;
	    (printf " up to {~a, ~a}~n" hit explore)
	    (if (null? (cdr result))
		(error 'search "didn't find a move!?")
		((cdr result))))))))

  ;; Simple search tests
  #;
  (let ([memory (make-hash-table 'equal)])
    (define (mv b p fi fj ti tj k)
      (move b p fi fj ti tj k void))
    (let loop ([limit 1])
      (define (go board)
	(minmax 0
		limit
		memory
		canonicalize
		'red
		'yellow
		(available-off-board board 'red)
		(available-off-board board 'yellow)
		board))
      (let ([v 
	     ;; One-step win
	     #;
	     (mv empty-board (list-ref red-pieces 2) #f #f 0 0
		 (lambda (board)
		   (mv board (list-ref red-pieces 2) #f #f 1 1
		       go)))
	     ;; 2-step win
	     #;
	     (mv empty-board (list-ref red-pieces 2) #f #f 0 0
		 (lambda (board)
		   (mv board (list-ref yellow-pieces 2) #f #f 1 0
		       (lambda (board)
			 (mv board (list-ref red-pieces 2) #f #f 1 1
			     (lambda (board)
			       (mv board (list-ref yellow-pieces 2) 1 0 2 2
				   (lambda (board)
				     (mv board (list-ref red-pieces 1) #f #f 1 0
					 (lambda (board)
					   (mv board (list-ref yellow-pieces 2) #f #f 1 0
					       (lambda (board)
						 (printf "~a~n" (board->string 0 board))
						 (go board)))))))))))))
	     ;; Hopeless case
	     #;
	     (mv empty-board (list-ref yellow-pieces 2) #f #f 0 0
		 (lambda (board)
		   (mv board (list-ref yellow-pieces 2) #f #f 1 1
		       (lambda (board)
			 (mv board (list-ref yellow-pieces 2) #f #f 0 1
			     (lambda (board)
			       (go board)))))))])
	(printf "~a ~a ~a~n" limit (car v) (if (null? (cdr v))
					       "???"
					       ((cdr v))))
	(loop (add1 limit)))))

  ;; Play-a-game test
  (let ([search (make-search)])
    (let loop ([board empty-board][who 'red])
      (cond
       [(winner? board who)
	(printf "~a wins!~n" who)]
       [(winner? board (other who))
	(printf "~a wins!~n" (other who))]
       [else
	(printf "~a~n~a~n" who (board->string 1 board))
	(let ([m (search 3.0 4 who board)])
	  (printf "Move ~a...~n" m)
	  (move board 
		(list-ref m 0)
		(list-ref m 1)
		(list-ref m 2)
		(list-ref m 3)
		(list-ref m 4)
		(lambda (new-board)
		  (loop new-board (other who)))
		(lambda ()
		  (error "bad move: ~a~n" m))))])))
  
  )
  