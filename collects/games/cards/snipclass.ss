(unit/sig cards:snipclass^
  (import mred^)

  (define sc (make-object snip-class%))
  (send sc set-classname "card")
  (send (get-the-snip-class-list) add sc))
