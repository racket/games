(unit/sig cards:region-local^
  (import)

  (rename (create-region make-region))

  (define-struct region (x y w h label callback button? hilite? decided-start? can-select?))

  (define create-region
    (lambda (x y w h label callback)
      (make-region x y w h label callback #f #f #f #f)))

  (define make-button-region
    (lambda (x y w h label callback)
      (make-region x y w h label callback #t #f #f #f))))
