(eval
 (let loop ([files (directory-list (collection-path
				    "games"
				    "paint-by-numbers"
				    "problems"))])
   (cond
     [(null? files)
      (unit/sig paint-by-numbers:all-problems^
	(import [p : paint-by-numbers:problem^])
	(define problemss null)
	(define set-names null))]
     [(equal? (car files) "CVS")
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
			   
			   (for-each 
			    (lambda (problem)
			      (unless (problem-solution problem)
			        (printf "No solution for ~a~n" (problem-name problem))))
			    new:problems)

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
	  (open combine)))])))
