(unit/sig SOLVE^
  (import mzlib:function^)
  
  (define (solve row-info col-info set-entry setup-progress)
    (local (
	    (define (pause) (sleep 1/16))
	    
	    ; all test cases are commented out.
	    
	    ; to work on large lists, we must make filter tail-recursive. 
	    ; this one reverses.
            
            ; filter-rev : returns a list of all elements in a-list which 
            ; satisfy the predicate.  If a precedes b in a-list, and both
            ; occur in the result, then b will precede a in the result.
            ; ((A -> boolean) (list-of A) -> (list-of A))
            
	    (define (filter-rev fun a-list)
	      (foldl (lambda (elt built-list) 
		       (if (fun elt) 
			   (cons elt built-list)
			   built-list))
		     null
		     a-list))
	    
	    ;(equal? (filter-rev (lambda (x)  (> x 13)) '(2 98 27 1 23 2 09))
            ;	    '(23 27 98))
	    
	    
	    ; transpose : transposes a matrix represented as a list of lists
	    ; ((list-of (list-of T)) -> (list-of (list-of T)))
	    
	    (define (transpose list-list)
	      (apply map list list-list))
	     
	    ;(equal? (transpose '((a b c d e)
            ;			 (f g h i j)
            ;			 (k l m n o)))
            ;	    '((a f k)
            ;         (b g l)
            ;	      (c h m)
            ;	      (d i n)
            ;	      (e j o)))
	    

            ; TYPE-DECLARATIONS:
            ; there are three kinds of cell-list: the board-row-list, the tally-list, and the try-list.
	    ;    
            ; (type: board-row (list-of (union 'off 'on 'unknown)))
            ; (type: tally-row (list-of (union 'off 'on 'unknown 'maybe-off 'maybe-on 'mixed)))
            ; (type: try-row (list-of (union 'maybe-off 'maybe-on)))
            ;    
            ; (type: board (list-of board-row))
            
	    ; board-ref : returns the board element in (col,row);
            ; (board num num -> (union 'on 'off 'unknown))
            
	    (define (board-ref board row col)
              (list-ref (list-ref board row)))
	    
	    ; extract-rows : returns the board as a list of rows
            ; (board -> board)
            
	    (define (extract-rows board)
	      board)
	    
	    ; extract-cols : returns the board as a list of columns
            ; (board -> board)
            
	    (define (extract-cols board)
	      (transpose board))
	    
            ; reassemble-rows : turns a list of rows into a board
            ; (board -> board)
            
	    (define (reassemble-rows board-line-list)
	      board-line-list)
	    
            ; reassemble-cols : turns a list of columns into a board
            ; (board -> board)
            
	    (define (reassemble-cols board-line-list)
	      (transpose board-line-list))
	    

            ; procedures to simplify the construction of test cases:
            
            ; condensed->long-form : takes a tree of short-form symbols and
            ; converts them to their long form, following this mapping:
            ; u -> unknown     |  X -> off
            ; ? -> maybe-on    |  O -> on
            ; ! -> maybe-off   |  * -> mixed
            
            (define (condensed->long-form symbol-tree)
              (cond [(cons? symbol-tree)
                     (cons (condensed->long-form (car symbol-tree))
                           (condensed->long-form (cdr symbol-tree)))]
                    [(case symbol-tree
                       ((u) 'unknown)
                       ((?) 'maybe-on)
                       ((!) 'maybe-off)
                       ((X) 'off)
                       ((O) 'on)
                       ((*) 'mixed)
                       ((()) ())
                       (else (error 'condensed->long-form "bad input: ~a" symbol-tree)))]))
            
            ;(equal? (condensed->long-form '(((? !) u) (* () X O)))
            ;        '(((maybe-on maybe-off) unknown) (mixed () off on)))
	    
	    ; check-changed : check whether a tally-row reveals new information to be added
            ; to the grid
            ; (tally-row -> boolean)
            
	    (define (check-changed tally-list)
	      (ormap (lambda (cell)
		       (case cell
			 ((off on unknown mixed) #f)
			 ((maybe-off maybe-on) #t)
			 (else (error "unknown element found in check-changed: ~a" cell))))
		     tally-list))
	    
            ;(and (equal? (check-changed '(off off on unknown mixed)) #f)
            ;	    (equal? (check-changed '(off on maybe-off on mixed)) #t)
            ;       (equal? (check-changed '(off maybe-on on on unknown)) #t))
	    
	    ; rectify : transform a tally-row into a board row, by changing maybe-off
            ; to off and maybe-on to on.
	    ; (tally-row -> board-row)

	    (define (rectify tally-list)
	      (map (lambda (cell)
		     (case cell
		       ((off on unknown) cell)
		       ((maybe-off) 'off)
		       ((maybe-on) 'on)
		       ((mixed) 'unknown)
		       (else (error "unknown element in rectified row"))))
		   tally-list))
	    
	    ;(equal? (rectify '(off on maybe-on mixed unknown maybe-off))
            ;	    '(off on on unknown unknown off))
	    
	    ; make-row-formulator:
	    ; given a set of block lengths, create a function which accepts a 
	    ; set of pads and formulates a try-row:
	    ; (num-list -> (num-list -> (list-of (union 'maybe-off 'maybe-on))))
	    
	    (define (make-row-formulator block-lens)
	      (lambda (pads)
		(append (build-list (car pads) (lambda (x) 'maybe-off))
			(apply 
			 append
			 (map (lambda (block-len pad-len)
				(append (build-list block-len (lambda (x) 'maybe-on))
					(build-list pad-len (lambda (x) 'maybe-off))))
			      block-lens
			      (cdr pads))))))
	    
	    #|
	    (equal? ((make-row-formulator '(3 1 1 5)) '(1 2 1 3 3))
		    '(maybe-off maybe-on maybe-on maybe-on maybe-off maybe-off maybe-on maybe-off maybe-on 
				maybe-off maybe-off maybe-off maybe-on maybe-on maybe-on maybe-on maybe-on
				maybe-off maybe-off maybe-off))
	    |#
	    
	    #| pad-pads: 
	    add one to every element of a list except the first and last
	    ((list-of num) -> (list-of num))
	    |#

	    (define (pad-pads num-list)
	      (cons (car num-list)
		    (let loop ((lon (cdr num-list)))
		      (cond
			((null? lon) null)
			((= (length lon) 1) lon)
			(else (cons (+ 1 (car lon)) (loop (cdr lon))))))))
	    
	    #|
	    (equal? (pad-pads '(3 1 2 4 3 2 1)) '(3 2 3 5 4 3 1))
	    |#
	    
	    #| check-try : 
	    see whether a try fits with the existing row information (curried)
	    (tally-row -> (try-row -> boolean))
	    |#
	    
	    (define (check-try tally-list)
	      (lambda (try-list)
		(andmap (lambda (tally try)
			  (case tally
			    ((off) (eq? try 'maybe-off))
			    ((on) (eq? try 'maybe-on))
			    (else #t)))
			tally-list
			try-list)))
	    
	    #|
	    (equal? ((check-try '(unknown off on unknown unknown unknown))
		     '(maybe-on maybe-on maybe-on maybe-off maybe-off maybe-off))
		    #f)
	    
	    (equal? ((check-try '(unknown off on unknown unknown unknown))
		     '(maybe-off maybe-off maybe-on maybe-on maybe-on maybe-off))
		    #t)
	    |#
	    
	    #| build-possibles:
	    builds a list of the possible rows.  given a number of spaces, and a number
	    of bins to put the spaces in, and a row-formulator, and a line-checker predicate,
	    build-possibles makes a list of every possible row which passes the predicate.
	    
	    (num num ((list-of num) -> try-row) (try-row -> bool) -> (list-of try-row))
	    |#
	    
	    (define (build-possibles things bins row-formulator line-checker)
	      (let ([built-list null])
		(let tree-traverse ([things things]
				    [bins bins]
				    [so-far null])
		  (if (= bins 1)
		      (let* ([this-try (cons things so-far)]
			     [padded (pad-pads this-try)]
			     [formulated (row-formulator padded)])
			(when (line-checker formulated)
			  (set! built-list (cons formulated built-list))))
		      (let try-loop ([in-this-bin 0])
			(if (> in-this-bin things)
			    #f
			    (begin
			      (tree-traverse (- things in-this-bin)
					     (- bins 1)
					     (cons in-this-bin so-far))
			      (try-loop (+ in-this-bin 1)))))))
		built-list))
	    
	    #| 
	    build-possibles test case
	    
	    (let* ([row-formulator-one (make-row-formulator '(2))]
		   [line-checker (check-try '(unknown unknown unknown on unknown unknown))]
		   [test-one (build-possibles 4 2 row-formulator-one line-checker)]
		   [row-formulator-two (make-row-formulator '(1 1))]
		   [test-two (build-possibles 3 3 row-formulator-two line-checker)])
	      (and (equal? test-one
			   '((maybe-off maybe-off maybe-on maybe-on maybe-off maybe-off)
			     (maybe-off maybe-off maybe-off maybe-on maybe-on maybe-off)))
		   (equal? test-two
			   '((maybe-on maybe-off maybe-off maybe-on maybe-off maybe-off)
			     (maybe-off maybe-on maybe-off maybe-on maybe-off maybe-off)
			     (maybe-off maybe-off maybe-off maybe-on maybe-off maybe-on)))))
	    
	    |#
	    
	    #| spare-spaces:
	    calculates the number of spare spaces in a line. In other words,
	    line-length - sum-of-all-blocks - spaces-between-blocks
	    
	    ((list-of num) num -> num)
	    |#
	    
	    (define (spare-spaces block-list line-length)
	      (let* ([black-spaces (apply + block-list)]
		     [inter-spaces (max (- (length block-list) 1) 0)]
		     [spare-spaces (- line-length (+ black-spaces inter-spaces))])
		spare-spaces))
	    
	    ; first-pass:
	    ; generates the information about row contents which can be inferred directly
	    ; from the block info and nothing else (i.e., uses no information from an existing
	    ; board.  
	    ; ((list-of (list-of num)) num -> (list-of (list-of (union 'on 'unknown))))
	    
	    (define (first-pass info-list line-length)
	      (let ((row-pass
		     (lambda (block-list)
		       (let* ([spares (spare-spaces block-list line-length)]
			      [shortened-blocks
			       (map (lambda (block-length) (- block-length spares))
				    block-list)]
			      [all-but-start
			       (foldr append null
				      (let build-row-loop ([blocks-left shortened-blocks])
					(if (null? blocks-left)
					    null
					    (let ([extra-pad (if (null? (cdr blocks-left)) 0 1)])
					      (if (> (car blocks-left) 0)
						  (cons (build-list (car blocks-left) (lambda (x) 'on))
							(cons (build-list (+ spares extra-pad) (lambda (x) 'unknown))
							      (build-row-loop (cdr blocks-left))))
						  (cons (build-list (+ spares extra-pad (car blocks-left))
								    (lambda (x) 'unknown))
							(build-row-loop (cdr blocks-left))))))))]
			      [whole-row (append (build-list spares (lambda (x) 'unknown))
						 all-but-start)])
			 whole-row))))
		(map row-pass info-list)))
	    
	    #|
	    (let ([test-result (first-pass '((4 3) (5 1)) 10)])
	      (equal? test-result '((unknown unknown on on unknown unknown unknown on unknown unknown)
				    (unknown unknown unknown on on unknown unknown unknown unknown unknown))))
	    |#
	    
	    #| unify-passes:
	    unify the result of running first-pass on both the rows and the columns
	    (let ([BOARD (list-of (list-of (union 'unknown 'on)))])
	      (BOARD BOARD -> BOARD))
	    |#
	    
	    (define (unify-passes board-a board-b)
	      (let ([unify-rows
		     (lambda (row-a row-b)
		       (map (lambda (cell-a cell-b)
			      (case cell-a
				((on) 'on)
				(else cell-b)))
			    row-a row-b))])
		(map unify-rows board-a board-b)))
	    
	    #|
	    (let* ([board-a '((unknown unknown on) (on unknown unknown))]
		   [board-b '((unknown on unknown) (on on unknown))]
		   [test-result (unify-passes board-a board-b)])
	      (equal? test-result '((unknown on on) (on on unknown))))
	    |#
	    
	    #| whole-first-pass:
	    take a set of row descriptions and the board dimensions and generate the 
	    merged first-pass info
	    ((list-of (list-of num)) (list-of (list-of num)) num num -> 
	     (list-of board-row))
	    |#
	    
	    (define (whole-first-pass row-info col-info width height)
	      (unify-passes (first-pass row-info width)
			    (transpose (first-pass col-info height))))
	    
	    #| memoize-tries:
	    given the black block widths and the line length and some initial board
	    and a progress-bar updater, calculate all possibilities for each row.
	    effect: updates the progress bar
	    ((list-of (list-of num)) num (list-of board-row) (-> void) -> (list-of try-row))
	    |#
	    
	    (define (memoize-tries info-list line-length board-rows update-progress)
	      (map (lambda (block-list board-row)
		     (update-progress)
		     (let ([spaces (spare-spaces block-list line-length)]
			   [bins (+ (length block-list) 1)]
			   [row-formulator (make-row-formulator block-list)]
			   [line-checker (check-try board-row)])
		       (build-possibles spaces bins row-formulator line-checker)))
		   info-list
		   board-rows))
	    
	    #|
	    (equal? (memoize-tries '((4) (1 3)) 
				   6 
				   '((unknown on unknown unknown unknown unknown)
				     (unknown off unknown unknown unknown unknown))
				   void)
		    '(((maybe-on maybe-on maybe-on maybe-on maybe-off maybe-off)
		       (maybe-off maybe-on maybe-on maybe-on maybe-on maybe-off))
		      ((maybe-on maybe-off maybe-on maybe-on maybe-on maybe-off)
		       (maybe-on maybe-off maybe-off maybe-on maybe-on maybe-on))))
	    |#
	    
	    #| batch-try: 
	    take a board-line list and a list of possibles, and trim it down by 
	    checking each try-list against the appropriate board-line
	    
	    ((list-of board-row) (list-of (list-of try-row)) -> (list-of (list-of try-row)))
	    |#
	    
	    (define (batch-try board-line-list try-list-list-list)
	      (map (lambda (line try-list-list)
		     (filter-rev (lambda (try-list) ((check-try line) try-list))
				 try-list-list))
		   board-line-list
		   try-list-list-list))
	    
	    #|
	    (equal? (batch-try '((unknown unknown unknown off)
				 (unknown on unknown unknown))
			       '(((maybe-on maybe-on maybe-on maybe-off)
				  (maybe-off maybe-on maybe-on maybe-on))
				 ((maybe-on maybe-on maybe-off maybe-off)
				  (maybe-off maybe-on maybe-on maybe-off)
				  (maybe-off maybe-off maybe-on maybe-on))))
		    '(((maybe-on maybe-on maybe-on maybe-off))
		      ((maybe-off maybe-on maybe-on maybe-off)
		       (maybe-on maybe-on maybe-off maybe-off))))
	    |#
	    
	    ; tabulate-try : take one possibility, and merge it with the row possibles
	    ; (tally-list try-list) -> null
	    
	    (define (tabulate-try tally-list try-list)
	      (map (lambda (tally try)
		     (case tally
		       ((off on mixed) tally)
		       ((unknown) try)
		       ((maybe-off maybe-on) (if (eq? try tally)
						 try
						 'mixed))
		       (else (error "unknown cell type during tabulate-try: ~a" tally))))
		   tally-list
		   try-list))
	    
	    
	    #|
	    (equal? (tabulate-try '(on off maybe-off maybe-off maybe-on maybe-on maybe-on)
				  '(on off mixed maybe-on maybe-on mixed maybe-off))
		    '(on off mixed mixed maybe-on mixed mixed))
	     |#
	    
	    ; batch-tabulate : take a board-line-list and a list of sets of tries which check with the board
	    ;  and tabulate them all to produce a new board line list (before rectification)
	    ; (board-line-list try-list-list-list) -> tally-list
	    (define (batch-tabulate board-line-list try-list-list-list)
	      (map (lambda (board-line try-list-list)
		     (foldl (lambda (x y) (tabulate-try y x)) board-line try-list-list))
		   board-line-list
		   try-list-list-list))
	    
	    
	    ;  (equal? (batch-tabulate '((unknown unknown unknown off)
	    ;			    (unknown unknown on unknown))
	    ;			  '(((maybe-on maybe-on maybe-off maybe-off)
	    ;			     (maybe-off maybe-on maybe-on maybe-off))
	    ;			    ((maybe-off maybe-on maybe-on maybe-off)
	    ;			     (maybe-off maybe-off maybe-on maybe-on))))
	    ;	  '((mixed maybe-on mixed off)
	    ;	    (maybe-off mixed on mixed)))
	    
	    (define (print-board board)
	      (for-each (lambda (row)
			  (for-each (lambda (cell)
				      (printf (case cell
						((off) " ")
						((unknown) ".")
						((on) "#"))))
				    row)
			  (printf "~n"))
			(extract-rows board)))
	    
	    ; animate-changes takes a board and draws it on the main screen
	    (define (animate-changes board draw-thunk outer-size inner-size)
	      (let outer-loop ([outer-index 0])
		(if (= outer-index outer-size)
		    null
		    (let inner-loop ([inner-index 0])
		      (if (= inner-index inner-size)
			  (begin
			    (pause)
			    (outer-loop (+ outer-index 1)))
			  (begin
			    (draw-thunk board outer-index inner-index)
			    (inner-loop (+ inner-index 1))))))))
	    
	    (define (draw-rows-thunk board row col)
	      (set-entry col row (board-ref board row col)))
	    
	    (define (draw-cols-thunk board col row)
	      (set-entry col row (board-ref board row col)))
	    
	    ;  (print-board '((on on unknown off)
	    ;		 (on on unknown unknown)
	    ;		 (unknown unknown on on)
	    ;		 (off unknown on on)))
	    
	    ; do-lines takes a board-line-list and a try-list-list-list and returns two things: a tally-list-list
	    ; and a new try-list-list-list
	    ; (board-line-list try-list-list-list) -> (tally-list-list try-list-list-list)
	    (define (do-lines board-line-list try-list-list-list)
	      (let ([new-tries (batch-try board-line-list try-list-list-list)])
		(values (batch-tabulate board-line-list new-tries)
			new-tries)))
	    
	    ; full-set takes a board and a pair of try-list-list-lists and returns a new board, a new pair
	    ; of try-list-list-lists, and a boolean (whether it's changed)
	    (define (full-set board row-try-list-list-list col-try-list-list-list)
	      (let*-values ([(board-rows new-row-tries)
			     (do-lines (extract-rows board) row-try-list-list-list)]
			    [(row-changed)
			     (ormap check-changed board-rows)]
			    [(new-board)
			     (reassemble-rows (map rectify board-rows))]
			    [( _ )
			     (if row-changed
				 (animate-changes new-board draw-rows-thunk 
						  (length board-rows)
						  (length (car board-rows))))]
			    [(board-cols new-col-tries)
			     (do-lines (extract-cols new-board) col-try-list-list-list)]
			    [(col-changed)
			     (ormap check-changed board-cols)]
			    [(final-board)
			     (reassemble-cols (map rectify board-cols))]
			    [( _ )
			     (if col-changed
				 (animate-changes final-board draw-cols-thunk
						  (length board-cols)
						  (length (car board-cols))))])
		(values final-board new-row-tries new-col-tries (or row-changed col-changed))))
	    
	    (define (local-solve row-info col-info)
	      (let* ([rows (length row-info)]
		     [cols (length col-info)]
		     [update-progress (setup-progress (+ rows cols))]
		     [initial-board (whole-first-pass row-info col-info cols rows)]
		     [row-try-list-list-list (memoize-tries row-info cols initial-board update-progress)]
		     [col-try-list-list-list (memoize-tries col-info rows (transpose initial-board) update-progress)]
		     [board (reassemble-rows initial-board)])
		(let loop ([board board] 
			   [row-tries row-try-list-list-list]
			   [col-tries col-try-list-list-list]
			   [changed #t])
		  (if changed
		      (call-with-values (lambda () (full-set board row-tries col-tries))
					loop)
		      board))))

	    #|
	    (let* ([row-info '((2)
			      (1)
			      (10 9)
			      (10 7 1)
			      (10 5 3)
			      (8 8)
			      (7 4 2)
			      (7 2 2 4)
			      (7 2 7)
			      (2 1)
			      (1)
			      (1 4)
			      (1 1 1)
			      (1 2 1 1 1)
			      (2 2 1 4 1)
			      (2 1 1 4 2)
			      (3 4 1 1)
			      (1 1)
			      (1 2 2 1)
			      (18))]
		  [col-info '((15 1)
			      (8 3)
			      (7 2 1)
			      (7 2 1)
			      (7 3 2)
			      (7 2)
			      (7 1)
			      (4 1)
			      (3 4 1)
			      (1 3 2 1 1 1)
			      (2 2 1 3 1)
			      (3 1 4 1)
			      (4 1 3 1)
			      (7 4 1)
			      (7 1)
			      (5 1 1)
			      (2 4 2)
			      (4 2 1 2)
			      (1 5 1 1)
			      (8 6))]
		  [initial-board (whole-first-pass row-info col-info 20 20)]
		  [col-try-list-list-list (memoize-tries col-info 20 initial-board void)])
	      (list-ref col-try-list-list-list 2))
	    |#
		  
	   
	    )
      (local-solve row-info col-info)
      )))