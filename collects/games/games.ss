(require-library "function.ss")

(invoke-unit/sig
 (unit/sig ()
   (import mred^ mzlib:function^)

   (define game-mapping 
     '(("same" "same.ss" "Same" #f)
       ("paint-by-numbers" "paint-by-numbers.ss" "Paint By Numbers" #f)
       ("gofish" "gofish.ss" "Go Fish" #t)
       ("blackjack" "blackjack.ss" "Blackjack" #t)
       ("ginrummy" "ginrummy.ss" "Gin Rummy" #t)
       ("mines" "mines.ss" "Minesweeper" #f)))
   
   (define f (make-object (class frame% (name)
			    (override
			      [on-close exit])
			    (sequence (super-init name)))
			  "PLT Games"))
   (send f set-alignment 'left 'top)
   (send f stretchable-width #f)
   (send f stretchable-height #f)

   (define m (make-object message% "Choose a game:" f))
   (define p (make-object vertical-panel% f))

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
			(when cards? (require-library "cards.ss" "games" "cards"))
			(let ([c (make-custodian)])
			  (parameterize ([current-custodian c])
			    (parameterize ([current-eventspace (make-eventspace)])
			      (queue-callback
			       (lambda ()
				 (exit-handler (lambda (v) 
						 (custodian-shutdown-all c)))
				 (load (build-path dir file))))))))))))

   (map game-button game-mapping)

   (let ([pred (lambda (x y) (<= (send x min-width) (send y min-width)))])
     (send p change-children (lambda (l) (quicksort l pred))))

   (send f show #t)

   (yield (make-semaphore 0)))
 mred^ mzlib:function^)


				      
