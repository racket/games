(eval
 `(let ([show-missing? #f]
	[total-missing 0])
    ,(let loop ([files
		 (call-with-input-file (build-path (collection-path
						    "games"
						    "paint-by-numbers"
						    "problems")
						   "directory")
		   read)])
       (cond
	[(null? files)
	 `(unit/sig paint-by-numbers:all-problems^
	    (import [p : paint-by-numbers:problem^])

	    (define problemss null)
	    (define set-names null))]
	[(or (equal? (car files) "CVS")
	     (not (file-exists? (build-path (collection-path "games" "paint-by-numbers" "problems") (car files)))))
	 (loop (cdr files))]
	[else 
	 `(compound-unit/sig 
	      (import [p : paint-by-numbers:problem^])
	    (link [new : paint-by-numbers:problem-set^ 
		       ((require-library ,(car files)
					 "games"
					 "paint-by-numbers"
					 "problems")
			p)]
		  [old : paint-by-numbers:all-problems^ (,(loop (cdr files)) p)]
		  [combine : paint-by-numbers:all-problems^
			   ((unit/sig paint-by-numbers:all-problems^
			      (import [old : paint-by-numbers:all-problems^]
				      [new : paint-by-numbers:problem-set^]
				      paint-by-numbers:problem^)
			      
			      (when show-missing?
				(for-each 
				 (lambda (problem)
				   (unless (problem-solution problem)
				     (set! total-missing (+ total-missing 1))
				     (printf "~a missing ~a: ~a~n" total-missing ,(car files) (problem-name problem))))
				 new:problems))

			      (define problemss 
				(if (null? new:problems)
				    old:problemss
				    (cons new:problems old:problemss)))
			      (define set-names
				(if (null? new:problems)
				    old:set-names
				    (cons new:set-name old:set-names))))
			    old
			    new
			    p)])
	    (export
	     (open combine)))]))))
