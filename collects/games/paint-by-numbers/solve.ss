(unit/sig SOLVE^
  (import [BOARD : BOARD^]
	  mzlib:function^)
  
  (define (pause) (sleep 1/16))

  ; all test cases are commented out.
  
  ; to work on large lists, we must make filter tail-recursive. 
  ; this one reverses.
  (define (filter-rev fun a-list)
    (foldl (lambda (elt built-list) 
	     (if (fun elt) 
		 (cons elt built-list)
		 built-list))
	   null
	   a-list))
  
  ;(equal? (filter-rev (lambda (x)  (> x 13)) '(2 98 27 1 23 2 09))
  ;    '(23 27 98))
  
  ; transpose : list-list -> list-list
  (define (transpose list-list)
    (apply map (lambda args (apply list args)) list-list))
  
  ; board-ref : board row col -> ['on 'off 'unknown]
  (define (board-ref board row col)
    (case (list-ref (list-ref board row) col)
      ((empty) 'off)
      ((full) 'on)
      ((unknown) 'unknown)))
  
  ; extract-rows : board -> board-line-list
  (define (extract-rows board)
    board)
  
  ; extract-cols : board -> board-line-list
  (define (extract-cols board)
    (transpose board))
  
  ;  (equal? (transpose '((a b c d e)
  ;		       (f g h i j)
  ;		       (k l m n o)))
  ;	  '((a f k)
  ;	    (b g l)
  ;	    (c h m)
  ;	    (d i n)
  ;	    (e j o)))
  
  (define (reassemble-rows board-line-list)
    board-line-list)
  
  (define (reassemble-cols board-line-list)
    (transpose board-line-list))
  
  ; there are three kinds of cell-list: the board-row-list, the tally-list, and the try-list.
  ; the board-row list = ['empty 'full 'unknown]*
  ; the tally-list = ['empty 'full 'unknown 'maybe-mt 'maybe-full 'mixed]*
  ; the try-list = ['maybe-mt 'maybe-full]*
  
  ; check-changed: tally-list -> boolean
  (define (check-changed tally-list)
    (ormap (lambda (cell)
	     (case cell
	       ((empty full unknown mixed) #f)
	       ((maybe-mt maybe-full) #t)
	       (else (error "unknown element found in check-changed: ~a" cell))))
	   tally-list))
  
  ;(and (equal? (check-changed '(empty empty full unknown mixed)) #f)
  ;     (equal? (check-changed '(empty full maybe-mt full mixed)) #t)
  ;     (equal? (check-changed '(empty maybe-full full full unknown)) #t))
  
  ; rectify:   tally-list -> board-row-list
  (define (rectify tally-list)
    (map (lambda (cell)
	   (case cell
	     ((empty full unknown) cell)
	     ((maybe-mt) 'empty)
	     ((maybe-full) 'full)
	     ((mixed) 'unknown)
	     (else (error "unknown element in rectified row"))))
	 tally-list))
  
  ; rectify test case
  ;(equal? (rectify '(empty full maybe-full mixed unknown maybe-mt))
;	  '(empty full full unknown unknown empty))
  
  ; formulate-row : using block lengths and a pad list, create a row list. 
  ; (num-list num-list) -> try-list
  (define (formulate-row block-lens pads)
    (append (build-list (car pads) (lambda (x) 'maybe-mt))
	    (apply 
	     append
	     (map (lambda (block-len pad-len)
		    (append (build-list block-len (lambda (x) 'maybe-full))
			    (build-list pad-len (lambda (x) 'maybe-mt))))
		  block-lens
		  (cdr pads)))))
  
  ;(equal? (formulate-row '(3 1 1 5) '(1 2 1 3 3))
;	  '(maybe-mt maybe-full maybe-full maybe-full maybe-mt maybe-mt maybe-full maybe-mt maybe-full 
;		     maybe-mt maybe-mt maybe-mt maybe-full maybe-full maybe-full maybe-full maybe-full
;		     maybe-mt maybe-mt maybe-mt))
  
  ; pad-pads: add one to every element of a list except the first and last
  ; num-list -> num-list
  (define (pad-pads num-list)
    (cons (car num-list)
	  (let loop ((lon (cdr num-list)))
	    (cond
	      ((empty? lon) null)
	      ((= (length lon) 1) lon)
	      (else (cons (+ 1 (car lon)) (loop (cdr lon))))))))
  
;  (equal? (pad-pads '(3 1 2 4 3 2 1)) '(3 2 3 5 4 3 1))
  
  ; distribute-try puts things into bins.  given a number of (identical) things, and a
  ; number of bins, distribute-try calls its thunk with every possible list representation
  ; of things in bins.
  ; distribute-try : (num num (num-list -> void) -> void)
  (define (distribute-try things bins thunk)
    (letrec ([inner-try 
	      (lambda (things bins built-list)
		(if (= bins 1)
		    (thunk (cons things built-list))
		    (let loop ((in-this-bin 0))
		      (if (> in-this-bin things)
			  null
			  (begin
			    (inner-try (- things in-this-bin) (- bins 1) (cons in-this-bin built-list))
			    (loop (+ in-this-bin 1)))))))])
      (inner-try things bins null)))
  
  ; distribute-try test : commented out because it prints a lot of gunk and seems to be right
  ;(distribute-try 5 3 (lambda (lon) (printf "~a~n" lon)))
  
  ;(let ([count 0])
  ;  (distribute-try 7 8 (lambda (x) (set! count (+ count 1))))
  ;  count)
  
  ; memoize-tries : 
  ; calculates all possibilties for this row, given the black block widths
  ; ((list-of (list-of num)) num (-> void) ->
  ;  (list (list-of (list-of (union 'maybe-full 'maybe-mt)))))
  ; effect : updates the progress bar

  ;; JOHN: Why is the result of this function always a list of length 1?

  ;; I have re-writtent the type for this function, using spidey's notation.
  ;; Here is the type bnf for spidey (in the subset I used):
  ;;
  ;;   T = (T ... T -> T) | void | num | (cons T T) | null
  ;;     | 'maybe-full | maybe-mt | ... any other symbol ...
  ;;     | (union T ... T)
  ;;     | (rec ([x T]) T)    (* recursive types -- x may appear free in either T *)
  ;;
  ;; additional type macros I used:
  ;;   (list T) = (cons T null)
  ;;   (list T T ... T) = (cons T (list T ... T))
  ;;   (list-of T) = (rec ([x (union null (cons T x))]) x)

  (define (memoize-tries info-list line-length update-progress)
    (map (lambda (info)
	   (let ([try-list-list null])
	     (distribute-try (- line-length (+ (apply + info)
					       (- (length info) 1)))
			     (+ (length info) 1)
			     (lambda (pad-list)
			       (set! try-list-list 
				     (cons (formulate-row info (pad-pads pad-list)) try-list-list))))
	     (update-progress)
	     try-list-list))
	 info-list))
  
;  (equal? (memoize-tries '((4) (1 3)) 6 void)
;	  '(((maybe-full maybe-full maybe-full maybe-full maybe-mt maybe-mt)
;	     (maybe-mt maybe-full maybe-full maybe-full maybe-full maybe-mt)
;	     (maybe-mt maybe-mt maybe-full maybe-full maybe-full maybe-full))
;	    ((maybe-full maybe-mt maybe-full maybe-full maybe-full maybe-mt)
;	     (maybe-full maybe-mt maybe-mt maybe-full maybe-full maybe-full)
;	     (maybe-mt maybe-full maybe-mt maybe-full maybe-full maybe-full))))
  
  ; check-try : see whether a try fits with the existing row information
  ; cell-list cell-list -> boolean
  (define (check-try tally-list try-list)
    (andmap (lambda (tally try)
	      (case tally
		((empty) (eq? try 'maybe-mt))
		((full) (eq? try 'maybe-full))
		(else #t)))
	    tally-list
	    try-list))
  
;  (equal? (check-try '(unknown empty full unknown unknown unknown)
;		     '(maybe-full maybe-full maybe-full maybe-mt maybe-mt maybe-mt))
;	  #f)
  
;  (equal? (check-try '(unknown empty full unknown unknown unknown)
;		     '(maybe-mt maybe-mt maybe-full maybe-full maybe-full maybe-mt))
;	  #t)
  
  ; batch-try: take a board-line list and a list of possibles, and trim it down by 
  ; checking each try-list against the appropriate board-line
  ; (board-line-list try-list-list-list -> try-list-list-list
  (define (batch-try board-line-list try-list-list-list)
    (map (lambda (line try-list-list)
	   (filter-rev (lambda (try-list) (check-try line try-list))
		       try-list-list))
	 board-line-list
	 try-list-list-list))
  
;  (equal? (batch-try '((unknown unknown unknown empty)
;		       (unknown full unknown unknown))
;		     '(((maybe-full maybe-full maybe-full maybe-mt)
;			(maybe-mt maybe-full maybe-full maybe-full))
;		       ((maybe-full maybe-full maybe-mt maybe-mt)
;			(maybe-mt maybe-full maybe-full maybe-mt)
;			(maybe-mt maybe-mt maybe-full maybe-full))))
;	  '(((maybe-full maybe-full maybe-full maybe-mt))
;	    ((maybe-mt maybe-full maybe-full maybe-mt)
;	     (maybe-full maybe-full maybe-mt maybe-mt))))
  
  ; tabulate-try : take one possibility, and merge it with the row possibles
  ; (tally-list try-list) -> null
  
  (define (tabulate-try tally-list try-list)
    (map (lambda (tally try)
	   (case tally
	     ((empty full mixed) tally)
	     ((unknown) try)
	     ((maybe-mt maybe-full) (if (eq? try tally)
					try
					'mixed))
	     (else (error "unknown cell type during tabulate-try: ~a" tally))))
	 tally-list
	 try-list))
  
  
  
 ; (equal? (tabulate-try '(full empty maybe-mt maybe-mt maybe-full maybe-full maybe-full)
;			'(full empty mixed maybe-full maybe-full mixed maybe-mt))
;	  '(full empty mixed mixed maybe-full mixed mixed))
  
  ; batch-tabulate : take a board-line-list and a list of sets of tries which check with the board
  ;  and tabulate them all to produce a new board line list (before rectification)
  ; (board-line-list try-list-list-list) -> tally-list
  (define (batch-tabulate board-line-list try-list-list-list)
    (map (lambda (board-line try-list-list)
	   (foldl (lambda (x y) (tabulate-try y x)) board-line try-list-list))
	 board-line-list
	 try-list-list-list))
  
  
;  (equal? (batch-tabulate '((unknown unknown unknown empty)
;			    (unknown unknown full unknown))
;			  '(((maybe-full maybe-full maybe-mt maybe-mt)
;			     (maybe-mt maybe-full maybe-full maybe-mt))
;			    ((maybe-mt maybe-full maybe-full maybe-mt)
;			     (maybe-mt maybe-mt maybe-full maybe-full))))
;	  '((mixed maybe-full mixed empty)
;	    (maybe-mt mixed full mixed)))
  
  (define (print-board board)
    (for-each (lambda (row)
		(for-each (lambda (cell)
			    (printf (case cell
				      ((empty) " ")
				      ((unknown) ".")
				      ((full) "#"))))
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
    (BOARD:set-entry col row (board-ref board row col)))
  
  (define (draw-cols-thunk board col row)
    (BOARD:set-entry col row (board-ref board row col)))
				 
;  (print-board '((full full unknown empty)
;		 (full full unknown unknown)
;		 (unknown unknown full full)
;		 (empty unknown full full)))
  
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
  
  (define (solve row-info col-info)
    (let* ([rows (length row-info)]
	   [cols (length col-info)]
	   [update-progress (BOARD:setup-progress (+ rows cols))]
	   [row-try-list-list-list (memoize-tries row-info cols update-progress)]
	   [col-try-list-list-list (memoize-tries col-info rows update-progress)]
	   [board (build-list rows (lambda (x) (build-list cols (lambda (x) 'unknown))))])
      (let loop ([board board] 
		 [row-tries row-try-list-list-list]
		 [col-tries col-try-list-list-list]
		 [changed #t])
	(if changed
	    (call-with-values (lambda () (full-set board row-tries col-tries))
			      loop)
	    board))))
  
  ;; JOHN -- this sanity check is applied in build problems.
  '(define (sanity-check row-info col-info)
    (= (apply + (map (lambda (x) (apply + x)) row-info))
       (apply + (map (lambda (x) (apply + x)) col-info)))))