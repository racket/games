(unit/sig cards:constants^
  (import mred^)

  (define ANIMATION-STEPS 5)
  (define ANIMATION-TIME 0.3)

  (define PRETTY-CARD-SEP-AMOUNT 5)

  (define red-brush
    (send the-brush-list
	  find-or-create-brush
	  "RED" 'solid))

  (define nice-font
    (send the-font-list
	  find-or-create-font
	  12 'decorative 'normal 'bold)))
