
(module constants mzscheme
  (require (lib "class.ss")
	   (lib "mred.ss" "mred"))

  (provide ANIMATION-STEPS
	   ANIMATION-TIME

	   PRETTY-CARD-SEP-AMOUNT
	   
	   red-brush
	   nice-font)
  
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
	  12 'decorative 'normal 'bold
	  #f 'default #t)))

