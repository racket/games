#!/bin/sh

string=? ; exec mzscheme -qgr $0

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

(require-library "function.ss")
(require-library "pretty.ss")

(require-library "errortrace.ss" "errortrace")

(define problems-dir (collection-path "games" "paint-by-numbers"))
(define input-file (build-path problems-dir "raw-problems.ss"))
(define output-file (build-path problems-dir "problems.ss"))

(define problems (call-with-input-file input-file (compose eval read)))

(define (sum-list l) (apply + l))
(define (sum-lists ls) (sum-list (map sum-list ls)))

;; perform sanity checks
(for-each (lambda (problem)
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
	  problems)

(call-with-output-file output-file
  (lambda (port)
    (parameterize ([current-output-port port])
      (pretty-print
       `(define problems (list
			  ,@(map (lambda (x)
				   `(make-problem
				     ,(first x)
				     ',(second x)
				     ',(third x)
				     #f))
				 problems))))))
  'truncate)
