
(define paint-by-numbers-loading-frame (make-object frame% "Paint By Numbers"))
(let ([p (make-object horizontal-panel% paint-by-numbers-loading-frame)])
  (make-object message% "Loading, please wait." p)
  (make-object grow-box-spacer-pane% p))
(send paint-by-numbers-loading-frame show #t)
(yield) (sleep) (yield) (sleep) (yield) (sleep) (yield) (sleep)

(require-library "sig.ss" "games" "paint-by-numbers")

(define paint-by-numbers-unit
  (compound-unit/sig (import)
    (link
     [C : mzlib:core^ ((require-library "corer.ss"))]
     [MRED : mred-interfaces^ (mred-interfaces@)]
     [FW : framework^ ((require-library "frameworkr.ss" "framework") C MRED)]
     [G : GUI^ ((require-library "gui.ss" "games" "paint-by-numbers") (C function) MRED)]
     [S : SOLVE^ ((require-library "solve.ss" "games" "paint-by-numbers") (C function))]
     [problem : paint-by-numbers:problem^ ((require-library "problem.ss" "games" "paint-by-numbers"))]
     [all : paint-by-numbers:all-problems^ ((require-library "all-problems.ss" "games" "paint-by-numbers") problem)]
     [M : BOARD^ ((require-library "main.ss" "games" "paint-by-numbers")
		  G S FW problem all (C pretty-print) MRED)])
    (export)))

(send paint-by-numbers-loading-frame show #f)
(invoke-unit/sig paint-by-numbers-unit)
