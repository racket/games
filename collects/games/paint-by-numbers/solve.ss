(unit/sig SOLVE^
  (import [MAIN : MAIN^]
	  mzlib:function^)
  
  (define (pause) (sleep 1))
  
  ;(include "solver.ss")
  
    ;;; JOHN, put your solver here. Call `pause' to pause. It will be a
    ;;; different function later, if we want to break the animation.
  
  (define (solve)
    (send MAIN:canvas set-rect 1 1 'on)
    (send MAIN:canvas paint-rect 1 1)))

