(require-library "sig.ss" "games" "paint-by-numbers")

(invoke-unit/sig
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
