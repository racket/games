(require-library "sig.ss" "games" "lights-out")

(invoke-unit/sig
 (compound-unit/sig
   (import [M : mred^])
   (link [F : mzlib:function^ ((require-library "functior.ss"))]
	 [B : lights-out:board^ 
            ((require-library "board.ss" "games" "lights-out") F M)]
	 [G : lights-out:gui^ ((require-library "gui.ss" "games" "lights-out")
                               M B)])
   (export))
 mred^)
