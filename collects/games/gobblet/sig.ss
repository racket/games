(module sig mzscheme
  (require (lib "unitsig.ss"))

  (provide config^ 
	   heuristic^ explore^
	   model^ restart^)

  (define-signature config^
    (BOARD-SIZE))

  (define-signature heuristic^
    (rate-board))

  (define-signature explore^
    (make-search apply-play))

  (define-signature model^
    (move 
     winner? 3-in-a-row?
     red-pieces yellow-pieces
     piece-color piece-size
     empty-board
     board-ref
     fold-board 
     fold-rowcol 
     other
     available-off-board
     make-canonicalize
     apply-xform unapply-xform))

  (define-signature restart^
    (new-game
     show-gobblet-help)))


