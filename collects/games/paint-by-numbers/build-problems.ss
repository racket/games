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

(require-library "functios.ss")
(require-library "prettys.ss")

(require-library "errortrace.ss" "errortrace")

(require-library "sig.ss" "games" "paint-by-numbers")

(if (eq? (vector) argv)
    (fprintf (current-error-port) "pass any commad line argument to skip the solver~n~n")
    (fprintf (current-error-port) "skipping the solver~n"))

(define BOARD
  (unit/sig BOARD^
    (import [SOLVE : SOLVE^]
	    mzlib:function^
	    mzlib:pretty-print^
	    (argv))

    (define problems-dir (collection-path "games" "paint-by-numbers"))
    (define input-file (build-path problems-dir "raw-problems.ss"))
    (define output-file (build-path problems-dir "problems.ss"))
    
    (define problems (call-with-input-file input-file (compose eval read)))

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

    (define (build-progress-outputer max)
      (let ([counter 0]
	    [dots-printed 0])
	(lambda ()
	  (set! counter (+ 1 counter))
	  (cond
	   [(= counter max)

	    ;; dots-printed should always equal progress-bar-max
	    (let loop ([n (- progress-bar-max dots-printed)])
	      (cond
	       [(zero? n) (void)]
	       [else (display "." (current-error-port))
		     (loop (- n 1))]))
	    (newline (current-error-port))]
	   [else
	    (let ([dots-to-print (floor (- (* progress-bar-max (/ counter (- max 1))) dots-printed))])
	      '(fprintf (current-error-port) "percentage: ~a ~a ~a ~a~n"
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

    (define (setup-progress max)
      (display guide-string (current-error-port))
      (newline (current-error-port))
      (build-progress-outputer max))

    (define time-limit 10) ;; in seconds

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
	  (set! solving-progress-output (build-progress-outputer (* row-count col-count))))
	(let* ([done (make-semaphore 0)]
	       [sucessful? #f]
	       [t (thread
		   (lambda ()
		     (SOLVE:solve rows cols)
		     (set! sucessful? #t)
		     (semaphore-post done)))])
	  (thread
	   (lambda ()
	     (sleep time-limit)
	     (kill-thread t)
	     (fprintf (current-error-port) "~ntime limit expired.~n")
	     (semaphore-post done)))
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
      			    
    ;; perform sanity checks before solving any boards
    (for-each sanity-check problems)

    (call-with-output-file output-file
      (lambda (port)
	(parameterize ([current-output-port port])
	  (pretty-print
	   `(define problems
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
		      problems))))))
      'truncate)))

(invoke-unit/sig
 (compound-unit/sig (import [A : (argv)])
   (link
    [F : mzlib:function^ ((require-library "functior.ss"))]
    [P : mzlib:pretty-print^ ((require-library "prettyr.ss"))]
    [S : SOLVE^ ((require-library "solve.ss" "games" "paint-by-numbers") B F)]
    [B : BOARD^ (BOARD S F P A)])
   (export))
 (argv))
