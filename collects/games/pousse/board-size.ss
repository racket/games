(module board-size mzscheme
  (define n 4)
  (define (set-n! v) (set! n v))
  (provide n set-n!))
