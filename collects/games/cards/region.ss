
(module region mzscheme
  (provide (rename create-region make-region)
	   region? region-x region-y region-w region-h 
	   region-label region-callback region-interactive-callback
	   region-button? region-hilite? region-decided-start? region-can-select?
	   set-region-hilite?! set-region-decided-start?! set-region-can-select?!
	   set-region-callback!
	   set-region-interactive-callback!
	   make-button-region)

  (define-struct region (x y w h label callback interactive-callback button? hilite? decided-start? can-select?))

  (define create-region
    (lambda (x y w h label callback)
      (unless (not (or (negative? w) (negative? h)))
	(error 'make-region "bad region size: ~a x ~a" w h))
      (make-region x y w h label callback #f #f #f #f #f)))

  (define make-button-region
    (lambda (x y w h label callback)
      (unless (not (or (negative? w) (negative? h)))
	(error 'make-button-region "bad region size: ~a x ~a" w h))
      (make-region x y w h label callback #f #t #f #f #f))))
