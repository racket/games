(unit/sig cards:main^
  (import [mred : mred^]
	  cards:classes^
	  cards:make-cards^)
  
  (define make-table
    (opt-lambda ([title "Cards"][w 7][h 3])
      (make-object table% title w h)))

  (define (make-deck)
    (map (lambda (l) (send l copy)) deck-of-cards)))
