(module games mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
	   (lib "class100.ss")
	   (lib "list.ss"))

   (define game-mapping 
     '(("slidey" "slidey.ss" "Slidey" #f)
       ("lights-out" "lights-out.ss" "Lights Out" #f)
       ("same" "same.ss" "Same" #f)
       ("paint-by-numbers" "paint-by-numbers.ss" "Paint By Numbers" #f)
       ("gofish" "gofish.ss" "Go Fish" #t)
       ("blackjack" "blackjack.ss" "Blackjack" #t)
       ("ginrummy" "ginrummy.ss" "Gin Rummy" #t)
       ("mines" "mines.ss" "Minesweeper" #f)
       ("pousse" "pousse.ss" "Pousse" #f)
       ("aces" "aces.scm" "Aces" #t)))
   
   (define f (make-object (class100 frame% (name)
			    (override
			      [on-close (lambda () (exit))])
			    (sequence (super-init name)))
			  "PLT Games"))
   (define hp (make-object horizontal-panel% f))
   (define main (make-object vertical-panel% hp))
   (send f set-alignment 'left 'top)
   (send f stretchable-width #f)
   (send f stretchable-height #f)

   (define m (make-object message% "Choose a game:" main))
   (define p (make-object vertical-panel% main))

   (define (game-button desc)
     (let* ([collect (car desc)]
	    [file (cadr desc)]
	    [name (caddr desc)]
	    [cards? (cadddr desc)]
	    [dir (with-handlers ([void (lambda (x) #f)])
		   (collection-path "games" collect))])
       (when dir
	 (make-object button% name p
		      (lambda (b e)
			(when cards? (dynamic-require '(lib "cards.ss" "games" "cards") #f))
			(let ([c (make-custodian)])
			  (parameterize ([current-custodian c])
			    (parameterize ([current-eventspace (make-eventspace)])
			      (queue-callback
			       (lambda ()
				 (exit-handler (lambda (v) 
						 (custodian-shutdown-all c)))
				 (dynamic-require `(file ,(build-path dir file)) #f)))))))))))

   (map game-button game-mapping)

   (let ([pred (lambda (x y) (<= (send x min-width) (send y min-width)))])
     (send p change-children (lambda (l) (quicksort l pred))))

   (make-object grow-box-spacer-pane% hp)
   
   (send f show #t)

   (yield (make-semaphore 0)))


				     