(module sig mzscheme
  (require (lib "unitsig.ss"))

  (provide config^ model^ restart^)

  (define-signature config^
    (BOARD-SIZE))

  (define-signature model^
    (move 
     winner? 3-in-a-row?
     red-pieces yellow-pieces
     piece-color piece-size
     empty-board
     board-ref
     fold-board 
     fold-rowcol 
     other))

  (define-signature restart^
    (new-game
     show-gobblet-help)))


