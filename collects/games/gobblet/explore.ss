(module explore mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "sig.ss")

  (provide explore-unit)

  (define explore-unit
    (unit/sig explore^
      (import config^ model^)

      (define (best span a b)
	(cond
	 [(zero? span) null]
	 [(null? a)
	  (if (null? b)
	      null
	      (cons (car b) (best (sub1 span) null (cdr b))))]
	 [(null? b)
	  (cons (car a) (best (sub1 span) null (cdr a)))]
	 [(> (caar a) (caar b))
	  (cons (car a) (best (sub1 span) (cdr a) b))]
	 [(and (= (caar a) (caar b))
	       (< (get-depth (car a)) (get-depth (car b))))
	  (cons (car a) (best (sub1 span) (cdr a) b))]
	 [else
	  (cons (car b) (best (sub1 span) a (cdr b)))]))

      (define (get-depth a)
	(if (vector? (cdr a))
	    (vector-ref (cdr a) 6)
	    0))

      (define no-goodness null)

      (define hit 0)
      (define explore 0)

      ;; -> (values (cons val move-thunk) xform)
      (define (minmax depth max-depth span memory canonicalize rate-board
		      me him my-pieces his-pieces board last-to-i last-to-j)
	(break-enabled #t)
	(break-enabled #f)
	(set! hit (add1 hit))
	(let* ([board-key+xform (canonicalize board me)]
	       [board-key (car board-key+xform)]
	       [xform (cdr board-key+xform)]
	       [key (vector board-key (- max-depth depth) span)]
	       [win/lose-key board-key])
	  (let ([goodness
		 (cond
		  ;; Check for known win at arbitrary depth:
		  [(hash-table-get memory win/lose-key (lambda () #f)) => (lambda (x) x)]
		  ;; Check for known value at specific max depth:
		  [(hash-table-get memory key (lambda () #f)) => (lambda (x) x)]
		  [(winner? board him)
		   (hash-table-put! memory win/lose-key '((-inf.0)))
		   '((-inf.0))]
		  [(winner? board me)
		   (hash-table-put! memory win/lose-key '((+inf.0)))
		   '((+inf.0))]
		  [(depth . >= . max-depth)
		   (let ([l (list (list (rate-board board me last-to-i last-to-j)))])
		     (hash-table-put! memory key l)
		     l)]
		  [else 
		   (set! explore (add1 explore))
		   ;; In case we get back here while we're looking, claim an unknown tie:
		   (hash-table-put! memory win/lose-key `((0 loop! ,me ,depth ,max-depth)))
		   (with-handlers ([exn:break? (lambda (x)
						 (hash-table-remove! memory win/lose-key)
						 (raise x))])
		     (let ([add-piece-goodness
			    (lambda (max-depth)
			      (let loop ([avail-pieces my-pieces]
					 [keep-pieces null]
					 [played-sizes null])
				(cond
				 [(null? avail-pieces) no-goodness]
				 [(memq (caar avail-pieces) played-sizes)
				  (loop (cdr avail-pieces)
					(cons (car avail-pieces) keep-pieces)
					played-sizes)]
				 [else
				  (best
				   span
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
							      (minmax (add1 depth) max-depth 1 memory canonicalize rate-board
								      him me his-pieces my-pieces new-board
								      i j)])
						  (best span
							v
							(list (cons (- (caar sv))
								    (vector p #f #f i j xform (add1 (get-depth (car sv)))))))))
					      (lambda () v)))
				      no-goodness)))])))]
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
								(minmax (add1 depth) max-depth 1 memory canonicalize rate-board
									him me his-pieces my-pieces new-board
									to-i to-j)])
						    
						    (best span
							  v 
							  (list (cons (- (caar sv))
								      (vector (car l) from-i from-j to-i to-j xform 
									      (add1 (get-depth (car sv)))))))))
						(lambda ()
						  v)))
					v)
				       v)))
			       no-goodness))])
		       (let ([goodness (let ([v (best span
						      (add-piece-goodness max-depth) 
						      (move-piece-goodness max-depth))])
					 (if (null? v)
					     '(-inf.0)
					     v))])
			 (hash-table-remove! memory win/lose-key)
			 (let ([key (if (and ((caar goodness) . < . +inf.0)
					     ((caar goodness) . > . -inf.0))
					;; Result is only valid to current depth limit:
					key
					;; Result is valid to any depth:
					win/lose-key)])
			   (hash-table-put! memory key goodness)
			   goodness))))])])
	    (values goodness xform))))

      (define (apply-play board m)
	(move board 
	      (list-ref m 0)
	      (list-ref m 1)
	      (list-ref m 2)
	      (list-ref m 3)
	      (list-ref m 4)
	      (lambda (new-board)
		new-board)
	      (lambda ()
		(error "bad move: ~a~n" m))))

      (define (multi-step-minmax steps one-step-depth span indent 
				 memory canonicalize rate-board me board)
	(define first-move? 
	  ((fold-board (lambda (i j v) (+ v (length (board-ref board i j)))) 0) . < . 2))
	(let-values ([(vs xform)
		      (minmax 0
			      one-step-depth
			      (if (or (steps . <= . 1) first-move?)
				  1
				  span)
			      memory
			      canonicalize
			      rate-board
			      me
			      (other me)
			      (if first-move?
				  ;; Ensure that we start with a large piece for first 2 moves:
				  (list (car (available-off-board board me)))
				  ;; Use any piece:
				  (available-off-board board me))
			      (available-off-board board (other me))
			      board #f #f)])
	  (let ([plays
		 (map (lambda (v)
			;; Transform each result:
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
				  (list (vector-ref x 0) from-i from-j to-i to-j)))))
		      (filter (lambda (v) (vector? (cdr v))) vs))])
	    (if (or (steps . <= . 1) first-move?)
		(car plays)
		(let ([nexts 
		       (if ((caar plays) . < . +inf.0)
			   (mergesort
			    (map
			     (lambda (play)
			       (if (= -inf.0 (car play))
				   (begin
				     #;
				     (printf " ~alosing: ~a\n" (make-string indent #\space) play)
				     play)
				   (let ([r (cons (- (car (multi-step-minmax (sub1 steps) span one-step-depth (add1 indent)
									     memory canonicalize rate-board
									     (other me)
									     (apply-play board (cdr play)))))
						  (cdr play))])
				     #;
				     (printf " ~atry ~a -> ~a\n" (make-string indent #\space) play (car r))
				     r)))
			     plays)
			    (lambda (a b)
			      (> (car a) (car b))))
			   (list (car plays)))])
		  (car nexts))))))

      (define (make-search)
	(lambda (timeout max-steps one-step-depth cannon-size rate-board me board)
	  (let ([result #f]
		[once-sema (make-semaphore)]
		[result-sema (make-semaphore)]
		[memory (make-hash-table 'equal)]
		[canonicalize (make-canonicalize cannon-size)])
	    (let ([t (thread
		      (lambda ()
			(break-enabled #f)
			(with-handlers ([exn:break? void])
			  (let loop ([steps 1])
			    (set! result
				  (let ([v (multi-step-minmax steps 3 one-step-depth 0
							      memory canonicalize rate-board
							      me board)])
				    #;
				    (printf " ~a -> ~a [~a]~n" 
					    steps
					    (cdr v)
					    (car v))
				    v))
			    (semaphore-post once-sema)
			    (unless (or (= steps max-steps)
					((car result) . = . +inf.0)
					((car result) . = . -inf.0))
			      (loop (add1 steps))))
			  (semaphore-post result-sema))))])
	      (sync/timeout timeout result-sema)
	      (semaphore-wait once-sema)
	      (break-thread t)
	      (sync t)
	      #;
	      (printf " up to {~a, ~a}~n" hit explore)
	      (if (null? (cdr result))
		  (error 'search "didn't find a move!?")
		  (cdr result))))))

      ;; Simple search tests
      #;
      (let ([memory (make-hash-table 'equal)])
	(define (mv b p fi fj ti tj k)
	  (move b p fi fj ti tj k void))
	(let loop ([limit 1])
	  (define (go board)
	    (minmax 0
		    limit
		    1
		    memory
		    canonicalize
		    (lambda args 0)
		    'red
		    'yellow
		    (available-off-board board 'red)
		    (available-off-board board 'yellow)
		    board #f #f))
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
	    (printf "~a ~a ~a~n" limit (caar v) (if (null? (cdar v))
						    "???"
						    ((cdar v))))
	    (loop (add1 limit))))))))
  