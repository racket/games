(require-library "sig.ss" "games" "paint-by-numbers")

(invoke-unit/sig
 (compound-unit/sig (import)
   (link
    [F : mzlib:function^ ((require-library "functior.ss"))]
    [MRED : mred^ (mred@)]
    [G : GUI^ ((require-library "gui.ss" "games" "paint-by-numbers") F MRED)]
    [S : SOLVE^ ((require-library "solve.ss" "games" "paint-by-numbers") M F)]
    [M : MAIN^ ((require-library "main.ss" "games" "paint-by-numbers") G S MRED)])
   (export)))
