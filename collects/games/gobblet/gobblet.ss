(module gobblet mzscheme
  (require (lib "unitsig.ss")
	   "sig.ss"
	   "model.ss"
	   "gui.ss")

  (provide game-unit)

  (define game-unit
    (unit/sig->unit
     (compound-unit/sig
      (import)
      (link [CONFIG : config^ ((unit/sig config^
				 (import)
				 (define BOARD-SIZE 3)))]
	    [MODEL : model^ (model-unit CONFIG)]
	    [GUI : () (gui-unit CONFIG MODEL)])
      (export)))))