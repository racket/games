(unit/sig cards:constants^
  (import [wx : wx^])

  (define ANIMATION-STEPS 5)
  (define ANIMATION-TIME 0.3)

  (define PRETTY-CARD-SEP-AMOUNT 5)

  (define red-brush
    (send wx:the-brush-list
	  find-or-create-brush
	  "RED" wx:const-solid))

  (define nice-font
    (send wx:the-font-list
	  find-or-create-font
	  12 wx:const-decorative wx:const-default wx:const-bold)))