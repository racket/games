#|

The paint-by-numbers-canavas% class accepts two initalization
arguments. They must be lists of lists of numbers and they must be the
same length. paint-by-numbers-canvas% objects accepts four methods:

  set-rect : (int int (union 'on 'off 'unknown) -> void)
    Sets the grid point specified by the first two arguments to the third.
    The coordinates are from the top-left and the x coordinate comes first.

  get-rect : (int int ->  (union 'on 'off 'unknown))
    Gets the value of the grid at the coordinates specified by the two integers

  paint-rect : (int int -> void)
    Draws the rectangle specified by the arguments.
    Call this after calling set-rect to see the changes updated on the screen.

  on-paint : (-> void)
    Redraws the entire canvas. May be used if many rects were set.

See the bottom of this file for the creation of a file and a test
paint by numbers.

|#

(require-library "sig.ss" "games" "paint-by-numbers")

(define GUI (require-library "gui.ss" "games" "paint-by-numbers"))

(define MAIN
  (unit/sig MAIN^

    (import [GUI : GUI^]
	    [SOLVE : SOLVE^]
	    mred^)

    (define-struct problem (name rows cols solution))
    (include "raw-problems.ss")

    (define frame (make-object frame% "Paint by Numbers"))
    (send frame stretchable-width #t)
    (send frame stretchable-height #t)

    (define top-panel (make-object horizontal-panel% frame))
    (define choice (make-object choice%
		     "Choose a Board"
		     (map problem-name problems)
		     top-panel
		     (lambda (choice evt)
		       (set-problem (list-ref problems (send choice get-selection))))))
    (define solve-button
      (make-object button%
	"Solve"
	top-panel
	(lambda (button evt) (SOLVE:solve))))
							    
    (define canvas #f)
    (define problem #f)
    
    (define (set-problem prlmb)
      (send frame change-children (lambda (x) (list top-panel)))
      (let ([rows (problem-rows prlmb)]
	    [cols (problem-cols prlmb)])
	(set! problem prlmb)
	(make-object message%
	  (format "The board is ~a cells wide and ~a cells tall" (length cols) (length rows))
	  frame)
	(set! canvas (make-object GUI:paint-by-numbers-canvas% frame rows cols))))

    (set-problem (car problems))
    (send choice set-selection 0)

    (send frame show #t)))

(invoke-unit/sig
 (compound-unit/sig (import)
   (link
    [F : mzlib:function^ ((require-library "functior.ss"))]
    [MRED : mred^ (mred@)]
    [G : GUI^ (GUI F MRED)]
    [S : SOLVE^ ((require-library "solve.ss" "games" "paint-by-numbers") M F)]
    [M : MAIN^ (MAIN G S MRED)])
   (export)))

