#!/bin/sh

string=? ; exec mzscheme -qgr $0 "$@"

#|

Simple shell script to read in the raw-problems.ss file and produce
problems.ss with no solutions. See raw-problems.ss for the description
of that file.

This file must produce code that evaluates to a list of problem
structs. The problem struct should have four fields: a string, a col,
a row and a (union #f solution)

The col and row type specs are in sig.ss and the solution type is not
yet defined.

|#

(require-library "macro.ss")
(require-library "functios.ss")
(require-library "prettys.ss")

(require-library "errortrace.ss" "errortrace")

(require-library "raw-sig.ss" "games" "paint-by-numbers")

(if (eq? (vector) argv)
    (fprintf (current-error-port) "pass any command line argument to skip the solver~n~n")
    (fprintf (current-error-port) "skipping the solver~n"))

(define BOARD
  (unit/sig BOARD^
    (import [SOLVE : SOLVE^]
	    mzlib:function^
	    mzlib:pretty-print^
	    (argv))

    (define time-limit (* 5 60)) ;; in seconds

    (define problems-dir (collection-path "games" "paint-by-numbers"))

    (define hattori-sets
      (let* ([set-size 30]
	     [raw-hattori
	      (call-with-input-file (build-path problems-dir "raw-hattori.ss")
		(compose eval read))]
	     [hattori-count (length raw-hattori)])
	(let o-loop ([n 0])
	  (cond
	   [(= n (- hattori-count 1)) null]
	   [else 
	    (let ([first n]
		  [last (if (< (+ n set-size) hattori-count)
			    (+ n set-size)
			    (- hattori-count 1))])
	      (let i-loop ([i first]
			   [set null])
		(cond
		 [(= i last) (cons 
			      (list (format "Hattori ~a - ~a" (+ first 1) last)
				    (format "h~a-~a" (+ first 1) last)
				    (reverse set))
			      (o-loop last))]
		 [else (i-loop (+ i 1)
			       (cons (list-ref raw-hattori i)
				     set))])))]))))

    (define (build-set name output-file input-file)
      (list name
	    output-file
	    (call-with-input-file (build-path problems-dir input-file) (compose eval read))))

    (define games-set (build-set "Games Magazine"
				 "games"
				 "raw-problems.ss"))
    (define misc-set (build-set "Misc"
				"misc"
				"raw-misc.ss"))

    ;(define sets (cons games-set (cons misc-set hattori-sets)))
    (define sets (list misc-set))

    (define (sum-list l) (apply + l))
    (define (sum-lists ls) (sum-list (map sum-list ls)))

    (define board #f)
    (define known 0)
    (define solving-progress-output void)

    (define (set-entry i j nv)
      (when (and (eq? (get-entry i j) 'unknown)
		 (not (eq? nv 'unknown)))
	(solving-progress-output))
      (vector-set! (vector-ref board i) j nv))

    (define (get-entry i j)
      (vector-ref (vector-ref board i) j))

    (define progress-bar-max 64)
    (define guide-string ".......:.......|.......:.......|.......:.......|.......:........")

    (define (build-progress-outputer max cleanup)
      (let ([counter 0]
	    [dots-printed 0])
	(lambda ()
	  (set! counter (+ 1 counter))
	  (cond
	   [(= counter max)

	    (cleanup)

	    ;; dots-printed should always equal progress-bar-max
	    (let loop ([n (- progress-bar-max dots-printed)])
	      (cond
	       [(zero? n) (void)]
	       [else (display "." (current-error-port))
		     (loop (- n 1))]))
	    (newline (current-error-port))]
	   [else
	    (let ([dots-to-print (floor (- (* progress-bar-max (/ counter (- max 1))) dots-printed))])
	      '(fprintf (current-error-port) "~spercentage: ~a ~a ~a ~a~n"
		       cleanup
		       dots-to-print
		       counter
		       (exact->inexact (/ counter max))
		       (exact->inexact (* progress-bar-max (/ counter max))))
	      (set! dots-printed (+ dots-to-print dots-printed))
	      (let loop ([n dots-to-print])
		(cond
		 [(zero? n) (void)]
		 [else
		  (display "." (current-error-port))
		  (loop (- n 1))]))
	      (flush-output (current-error-port)))]))))

    (define (setup-progress max cleanup)
      (display guide-string (current-error-port))
      (newline (current-error-port))
      (build-progress-outputer max cleanup))

    (define (solve name rows cols)
      (cond
       [(equal? argv (vector))
	(fprintf (current-error-port) "Solving ~s; time limit ~a seconds~n" name time-limit)
	(let ([row-count (length rows)]
	      [col-count (length cols)])
	  (set! board
		(build-vector col-count
			      (lambda (i) (make-vector row-count 'unknown))))
	  (set! known 0)
	  (set! solving-progress-output (build-progress-outputer
					 (* row-count col-count)
					 void)))
	(letrec ([done (make-semaphore 0)]
		 [kill (make-semaphore 1)]
		 [sucessful? #f]
		 [t (thread
		     (lambda ()
		       (with-handlers ([(lambda (x) #t)
					(lambda (x)
					  (semaphore-wait kill)
					  (set! sucessful? #f)
					  (kill-thread k)
					  (fprintf (current-error-port) "~nsolver raised an exception~n~a~n"
						  (if (exn? x)
						      (exn-message x)
						      x))
					  (semaphore-post done))])
			 (SOLVE:solve rows cols set-entry
				      (lambda (max)
					(setup-progress
					 max
					 (lambda ()
					   (semaphore-wait kill)
					   (set! sucessful? #t)
					   (kill-thread k)))))
			 (semaphore-post done))))]
		 [k
		  (thread
		   (lambda ()
		     (sleep time-limit)
		     (semaphore-wait kill)
		     (kill-thread t)
		     (fprintf (current-error-port) "~ntime limit expired.~n")
		     (semaphore-post done)))])
	  (semaphore-wait done)
	  (newline (current-error-port))
	  (newline (current-error-port))
	  (if sucessful?
	      board
	      #f))]
       [else #f]))

    (define (sanity-check problem)
      (let ([name (first problem)]
	    [cols (second problem)]
	    [rows (third problem)])
	(when (null? cols)
	  (error 'build-problems.ss
		 "problem ~a doesn't have any cols" name))
	(when (null? rows)
	  (error 'build-problems.ss
		 "problem ~a doesn't have any rows" name))
	(unless (= (sum-lists (second problem)) (sum-lists (third problem)))
	  (error 'build-problems.ss
		 "problem ~a: sum of the column lists is not the same as the sum of the row lists"
		 name))))
      			    
    (for-each 
     (lambda (set)
       (let ([set-name (car set)]
	     [output-file (build-path (collection-path "games" "paint-by-numbers" "problems")
				      (cadr set))]
	     [problems (caddr set)])
	 (for-each sanity-check problems)
	 (if (file-exists? output-file)
	     (printf "skipping ~s~n" set-name)
	     (call-with-output-file output-file
	       (lambda (port)
		 (printf "Building ~s~n" set-name)
		 (parameterize ([current-output-port port])
		   (pretty-print
		    `(unit/sig paint-by-numbers:problem-set^
			       (import paint-by-numbers:problem^)

			       (define set-name ,set-name)

			       (define problems
				 (list
				  ,@(map (lambda (x)
					   (let ([name (first x)]
						 [rows (second x)]
						 [cols (third x)])
					     `(make-problem
					       ,(first x)
					       ',rows
					       ',cols
					       ',(solve name rows cols))))
					 problems)))))))))))
     sets)))

(invoke-unit/sig
 (compound-unit/sig (import [A : (argv)])
   (link
    [F : mzlib:function^ ((require-library "functior.ss"))]
    [P : mzlib:pretty-print^ ((require-library "prettyr.ss"))]
    [S : SOLVE^ ((require-library "solve.ss" "games" "paint-by-numbers") F)]
    [B : BOARD^ (BOARD S F P A)])
   (export))
 (argv))
