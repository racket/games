(unit/sig cards:region-local^
  (import)

  (rename (create-region make-region))

  (define-struct region (x y w h label callback button? hilite? decided-start? can-select?))

  (define create-region
    (lambda (x y w h label callback)
      (unless (not (or (negative? w) (negative? h)))
	(error 'make-region "bad region size: ~a x ~a" w h))
      (make-region x y w h label callback #f #f #f #f)))

  (define make-button-region
    (lambda (x y w h label callback)
      (unless (not (or (negative? w) (negative? h)))
	(error 'make-button-region "bad region size: ~a x ~a" w h))
      (make-region x y w h label callback #t #f #f #f))))
