(unit/sig cards:snipclass^
  (import [wx : wx^])

  (define sc (make-object wx:snip-class%))
  (send sc set-classname "card")
  (send (wx:get-the-snip-class-list) add sc))
