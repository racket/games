(module sig mzscheme
  (require (lib "unitsig.ss"))

  (provide config^ model^)

  (define-signature config^
    (BOARD-SIZE))

  (define-signature model^
    (move 
     winner?
     red-pieces yellow-pieces
     piece-color piece-size
     empty-board
     board-ref
     fold-board 
     fold-rowcol 
     other)))

