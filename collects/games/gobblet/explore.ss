(module explore mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "sig.ss")

  (provide explore-unit)

  (define-syntax (log-printf stx)
    (syntax-case stx ()
      [(_ n i arg ...)
       (<= (syntax-e #'n) 0)
       #'(begin
	   (when (i  . < . 100)
	     (printf arg ...))
	   (void))]
      [(_ n i arg ...)
       #'(void)]))

  (define explore-unit
    (unit/sig explore^
      (import config^ model^)

      (define delay-loss? #t)

      (define learn? #f)
      (define MEMORY-FILE (and learn?
			       (build-path (find-system-path 'addon-dir)
					   (format "gobblet-memory-~a.ss" BOARD-SIZE))))

      (define-struct plan (size from-i from-j to-i to-j xform turns))

      (define (xlate m xform)
	(let-values ([(from-i from-j)
		      (if (plan-from-i m)
			  (unapply-xform xform (apply-xform (plan-xform m)
							    (plan-from-i m)
							    (plan-from-j m)))
			  (values #f #f))]
		     [(to-i to-j)
		      (unapply-xform xform (apply-xform (plan-xform m)
							(plan-to-i m)
							(plan-to-j m)))])
	  (make-plan (plan-size m) from-i from-j to-i to-j xform (plan-turns m))))


      (define (float->string v)
	(let ([s (string-append (number->string v) "000000")])
	  (substring s 0 (min 6 (string-length s)))))

      (define (play->string p)
	(format "~a (~a,~a)->(~a,~a) [~a/~a]"
		(piece-size (list-ref p 1))
		(list-ref p 2) (list-ref p 3)  (list-ref p 4) (list-ref p 5)
		(float->string (car p))
		(list-ref p 6)))

      (define (plays->string is p)
	(if (null? p)
	    "()"
	    (let ([s (plays->string is (cdr p))])
	      (if (null? (cdr p))
		  (play->string (car p))
		  (string-append (play->string (car p))
				 "\n"
				 is
				 s)))))

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
	 [(= (caar a) (caar b))
	  (cond
	   [(and delay-loss? 
		 (= (caar a) -inf.0))
	    (if (> (get-depth (car a)) (get-depth (car b)))
		(cons (car a) (best (sub1 span) (cdr a) b))
		(cons (car b) (best (sub1 span) a (cdr b))))]
	   [(< (get-depth (car a)) (get-depth (car b)))
	    (cons (car a) (best (sub1 span) (cdr a) b))]
	   [else (cons (car b) (best (sub1 span) a (cdr b)))])]
	 [else
	  (cons (car b) (best (sub1 span) a (cdr b)))]))

      (define (found-win? v)
	(and (pair? v)
	     (= (caar v) +inf.0)))

      (define (immediate? v)
	(and (pair? v) (zero? (get-depth (car v)))))

      (define (found-lose? v)
	(and (pair? v)
	     (= (caar v) -inf.0)))

      (define (get-depth a)
	(if (plan? (cdr a))
	    (plan-turns (cdr a))
	    0))

      (define no-goodness null)

      (define hit-count 0)
      (define depth-count 0)
      (define explore-count 0)
      (define enter-count 0)
      (define move-count 0)

      ;; Discourage loops:
      (define LOOP-TIE `((-1000.0 loop!)))

      (define (show-recur sz from-i from-j to-i to-j sv)
	(if (not (plan? (cdar sv)))
	    (printf "   Recur ~a (~a,~a)->(~a,~a) ; ??? = ~a/~a~n"
		    sz from-i from-j to-i to-j 
		    (caar sv) (get-depth (car sv)))
	    (printf "   Recur ~a (~a,~a)->(~a,~a) ; (~a,~a)->(~a,~a) = ~a/~a~n"
		    sz from-i from-j to-i to-j 
		    (plan-from-i (cdar sv)) (plan-from-j (cdar sv)) (plan-to-i (cdar sv)) (plan-to-j (cdar sv))
		    (caar sv) (get-depth (car sv)))))

      ;; -> (values (cons val plan) xform)
      (define (minmax depth max-depth span memory canonicalize 
		      rate-board canned-moves
		      me him my-pieces his-pieces board last-to-i last-to-j)
	(set! hit-count (add1 hit-count))
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
		  [(begin (set! depth-count (add1 depth-count)) #f) (void)]
		  [(depth . >= . max-depth)
		   (let ([l (list (list (rate-board board me last-to-i last-to-j)))])
		     (hash-table-put! memory key l)
		     l)]
		  [else 
		   (set! explore-count (add1 explore-count))
		   ;; In case we get back here while we're looking, claim an unknown tie:
		   (hash-table-put! memory win/lose-key LOOP-TIE)
		   (let* ([canned-goodness
			   (map (lambda (g)
				  ;; Make sure each canned move is in our coordinate system:
				  (cons (car g) (xlate (cdr g) xform)))
				(canned-moves board me board-key xform))]
			  [add-piece-goodness
			   (if (found-win? canned-goodness)
			       canned-goodness
			       (let loop ([avail-pieces my-pieces]
					  [keep-pieces null]
					  [played-sizes 
					   ;; Don't consider sizes that appear in known moves:
					   (let loop ([sizes null][canned-goodness canned-goodness])
					     (cond
					      [(null? canned-goodness) sizes]
					      [(not (plan-from-j (cdar canned-goodness)))
					       (let ([sz (plan-size (cdar canned-goodness))])
						 (if (member sz sizes)
						     (loop sizes (cdr canned-goodness))
						     (loop (cons sz sizes) (cdr canned-goodness))))]
					      [else (loop sizes (cdr canned-goodness))]))])
				 (cond
				  [(null? avail-pieces) canned-goodness]
				  [(memq (caar avail-pieces) played-sizes)
				   (loop (cdr avail-pieces)
					 (cons (car avail-pieces) keep-pieces)
					 played-sizes)]
				  [else
				   (let ([v
					  ;; Try other pieces:
					  (loop (cdr avail-pieces)
						(cons (car avail-pieces) keep-pieces)
						(cons (caar avail-pieces) played-sizes))])
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
					  (if (and (found-win? v) (immediate? v))
					      v
					      (move board p #f #f i j
						    (lambda (new-board)
						      (set! enter-count (add1 enter-count))
						      (let-values ([(sv sxform)
								    (minmax (add1 depth) max-depth 1 memory canonicalize 
									    rate-board canned-moves
									    him me his-pieces my-pieces new-board
									    i j)])
							#;
							(when (zero? depth)
							  (show-recur (piece-size p) #f #f i j sv))
							(best span
							      v
							      (list (cons (- (caar sv))
									  (make-plan (piece-size p) #f #f i j 
										     xform (add1 (get-depth (car sv)))))))))
						    (lambda () v))))
					v)))])))]
			  [move-piece-goodness
			   (if (found-win? canned-goodness)
			       canned-goodness
			       (fold-board
				(lambda (from-i from-j v)
				  (let ([l (board-ref board from-i from-j)])
				    (if (and (pair? l)
					     (eq? me (piece-color (car l))))
					(fold-board
					 (lambda (to-i to-j v)
					   (if (or (and (found-win? v) (immediate? v))
						   (ormap (lambda (km)
							    (let ([m (cdr km)])
							      (and (equal? from-i (plan-from-i m))
								   (equal? from-j (plan-from-j m))
								   (equal? to-i (plan-to-i m))
								   (equal? to-j (plan-to-j m)))))
							  canned-goodness))
					       v
					       (move board (car l) from-i from-j to-i to-j
						     (lambda (new-board)
						       (set! move-count (add1 move-count))
						       (let-values ([(sv sxform)
								     (minmax (add1 depth) max-depth 1 memory canonicalize
									     rate-board canned-moves
									     him me his-pieces my-pieces new-board
									     to-i to-j)])
							 #;
							 (when (zero? depth)
							   (show-recur (piece-size (car l)) from-i from-j to-i to-j sv))
							 (best span
							       v 
							       (list (cons (- (caar sv))
									   (make-plan (piece-size (car l)) from-i from-j to-i to-j 
										      xform 
										      (add1 (get-depth (car sv)))))))))
						     (lambda ()
						       v))))
					 v)
					v)))
				add-piece-goodness))])
		     (let ([goodness (let ([v move-piece-goodness])
				       (if (null? v)
					   '((-inf.0))
					   v))])
		       (hash-table-remove! memory win/lose-key)
		       (let ([key (if (and ((caar goodness) . < . +inf.0)
					   ((caar goodness) . > . -inf.0))
				      ;; Result is only valid to current depth limit:
				      key
				      ;; Result is valid to any depth:
				      win/lose-key)])
			 (hash-table-put! memory key goodness)
			 goodness)))])])
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
		(error 'apply-play "bad move: ~a" m))))

      (define (multi-step-minmax steps one-step-depth span indent 
				 memory init-memory canonicalize 
				 rate-board canned-moves
				 me board)
	(define first-move? 
	  ((fold-board (lambda (i j v) (+ v (length (board-ref board i j)))) 0) . < . 2))
	(define now (current-inexact-milliseconds))
	(set! hit-count 0)
	(set! depth-count 0)
	(set! explore-count 0)
	(set! enter-count 0)
	(set! move-count 0)
	(log-printf 1 indent "~a> ~a Exploring for ~a~n" (make-string indent #\space) steps me)
	(let-values ([(vs xform)
		      (minmax 0
			      one-step-depth
			      (if (or (steps . <= . 1) first-move?)
				  1
				  span)
			      memory canonicalize
			      rate-board canned-moves
			      me
			      (other me)
			      (available-off-board board me)
			      (available-off-board board (other me))
			      board #f #f)])
	  (log-printf 2 indent "~a>> Done ~a ~a ~a ~a+~a [~a secs]~n" 
		      (make-string indent #\space) 
		      hit-count depth-count explore-count enter-count move-count
		      (float->string (/ (- (current-inexact-milliseconds) now) 1000)))
	  (let ([plays
		 (map (lambda (v)
			;; Transform each result, and turn it into a list
			(cons (car v) 
			      (let ([m (xlate (cdr v) xform)])
				(list (list-ref (if (eq? me 'red)
						    red-pieces
						    yellow-pieces)
						(plan-size m))
				      (plan-from-i m)
				      (plan-from-j m)
				      (plan-to-i m)
				      (plan-to-j m)
				      (get-depth v)))))
		      (filter (lambda (v) (plan? (cdr v))) vs))])
	    (log-printf 3 indent "~a>> Best Plays: ~a\n" 
			(make-string indent #\space) (plays->string 
						      (make-string (+ 15 indent) #\space)
						      plays))

	    ;; Record what we've learned...
	    (when (and learn?
		       (= steps 1))
	      (when (or (found-win? plays)
			(found-lose? plays))
		(let ([board-key+xform (canonicalize board me)])
		  (hash-table-get init-memory 
				  (car board-key+xform)
				  (lambda ()
				    ;; This is new...
				    (with-output-to-file MEMORY-FILE
				      (lambda ()
					(let ([m (cdar plays)])
					  (printf "(~a ~a ~a)~n#|~n~a|#~n" 
						  (if (found-win? plays) 'win 'lose)
						  (car board-key+xform)
						  (list
						   (piece-color (list-ref m 0))
						   (piece-size (list-ref m 0))
						   (list-ref m 1) (list-ref m 2) (list-ref m 3) (list-ref m 4)
						   (list-ref m 5))
						  (board->string 0 board))))
				      'append))))))

	    (if (or (steps . <= . 1) first-move?)
		(car plays)
		(let ([nexts 
		       (if ((caar plays) . < . +inf.0)
			   (mergesort
			    (map
			     (lambda (play)
			       (log-printf 4 indent " ~a>>> Checking: ~a\n" 
					   (make-string indent #\space) (play->string play))
			       (if (= -inf.0 (car play))
				   (begin
				     (log-printf 4 indent " ~a>>>> losing\n" (make-string indent #\space))
				     play)
				   (let ([r (cons (- (car (multi-step-minmax 
							   (sub1 steps) one-step-depth span (+ 3 indent)
							   memory init-memory canonicalize 
							   rate-board canned-moves
							   (other me)
							   (apply-play board (cdr play)))))
						  (cdr play))])
				     (log-printf 4 indent " ~a>>>> deeper = ~a\n" 
						 (make-string indent #\space) (float->string (car r)))
				     r)))
			     plays)
			    (lambda (a b)
			      (> (car a) (car b))))
			   (list (car plays)))])
		  (car nexts))))))

      ;; to laod what we've learned
      (define (load-memory init-memory canonicalize)
	(with-handlers ([exn:fail:filesystem? void])
	  (with-input-from-file MEMORY-FILE
	    (lambda ()
	      (let loop ()
		(let ([v (read)])
		  (unless (eof-object? v)
		    (let ([board-key+xform (canonicalize (cadr v) #f)])
		      (hash-table-put! init-memory
				       (car board-key+xform)
				       (list
					(cons (if (eq? 'win (car v)) +inf.0 -inf.0)
					      (let ([n (caddr v)])
						(make-plan
						 (cadr n)
						 (list-ref n 2) (list-ref n 3) (list-ref n 4) (list-ref n 5)
						 (cdr board-key+xform)
						 (list-ref n 6)))))))
		    (loop))))))))

      (define (make-search make-rate-board make-canned-moves)
	(define init-memory (make-hash-table 'equal))
	(define canonicalize (make-canonicalize))
	(define rate-board (make-rate-board canonicalize))
	(define canned-moves (make-canned-moves canonicalize init-memory))
	(when learn?
	  (load-memory init-memory canonicalize))
	(lambda (timeout max-steps one-step-depth
			 me board history)
	  (let* ([result #f]
		 [once-sema (make-semaphore)]
		 [result-sema (make-semaphore)]
		 [memory (make-hash-table 'equal)])
	    ;; Record game-history boards as loop ties
	    (let loop ([history history][me (other me)])
	      (unless (null? history)
		(let ([key+xform (canonicalize (car history) me)])
		  (hash-table-put! memory (car key+xform) LOOP-TIE))
		(loop (cdr history) (other me))))
	    (hash-table-for-each init-memory (lambda (k v) (hash-table-put! memory k v)))
	    (let ([t (thread
		      (lambda ()
			(let loop ([steps 1])
			  (set! result
				(let ([v (multi-step-minmax steps
							    one-step-depth
							    3 ; span
							    0 ; indent 
							    memory init-memory canonicalize 
							    rate-board canned-moves
							    me board)])
				  (log-printf 1 0 "> ~a Result: ~a~n" 
					      steps
					      (play->string v))
				  v))
			  (semaphore-post once-sema)
			  (unless (or (= steps max-steps)
				      ((car result) . = . +inf.0)
				      ((car result) . = . -inf.0))
			    (loop (add1 steps))))
			(semaphore-post result-sema)))])
	      (sync/timeout timeout result-sema)
	      (semaphore-wait once-sema)
	      (kill-thread t)
	      (when (null? (cdr result))
		(error 'search "didn't find a move!?"))
	      (cdr result)))))

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
  