(define paint-by-numbers-loading-frame (make-object frame% "Paint By Numbers"))
(make-object message% "Loading, please wait." paint-by-numbers-loading-frame)
(send paint-by-numbers-loading-frame show #t)
(yield)

(require-library "sig.ss" "games" "paint-by-numbers")

(define paint-by-numbers-unit
  (compound-unit/sig (import)
    (link
     [C : mzlib:core^ ((require-library "corer.ss"))]
     [MRED : mred-interfaces^ (mred-interfaces@)]
     [FW : framework^ ((require-library "frameworkr.ss" "framework") C MRED)]
     [G : GUI^ ((require-library "gui.ss" "games" "paint-by-numbers") (C function) MRED)]
     [S : SOLVE^ ((require-library "solve.ss" "games" "paint-by-numbers") (C function))]
     [M : BOARD^ ((require-library "main.ss" "games" "paint-by-numbers")
		  G S FW (C pretty-print) MRED)])
    (export)))

(send paint-by-numbers-loading-frame show #f)
(invoke-unit/sig paint-by-numbers-unit)
