(module games mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "unit.ss")
	   (lib "class100.ss")
	   (lib "list.ss")
           (lib "getinfo.ss" "setup")
	   "show-help.ss")

   (define game-mapping 
     (let ([games (let ([d (collection-path "games")])
                    (filter (lambda (f)
                              (let ([p (build-path d f)])
                                (and (directory-exists? p)
                                     (with-handlers ([not-break-exn? (lambda (x) #f)])
                                       ((get-info (list "games" f)) 'game (lambda () #f))))))
                            (directory-list d)))])
       (map (lambda (g)
              (let ([info (get-info `("games" ,g))])
                (list g 
                      (info 'game (lambda () "wrong.ss"))
                      (info 'name (lambda () g)))))
            games)))
               
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
	    [dir (with-handlers ([void (lambda (x) #f)])
		   (collection-path "games" collect))])
       (when dir
	 (make-object button% name p
		      (lambda (b e)
			(let ([game-unit (dynamic-wind
                                          begin-busy-cursor
                                          (lambda () (dynamic-require `(file ,(build-path dir file)) 'game-unit))
                                          end-busy-cursor)])
                          (let ([c (make-custodian)])
                            (parameterize ([current-custodian c])
                              (parameterize ([current-eventspace (make-eventspace)])
                                (queue-callback
                                 (lambda ()
                                   (exit-handler (lambda (v) 
                                                   (custodian-shutdown-all c)))
                                   (invoke-unit game-unit))))))))))))

   (map game-button game-mapping)

   (let ([pred (lambda (x y) (<= (send x min-width) (send y min-width)))])
     (send p change-children (lambda (l) (quicksort l pred))))

   (make-object grow-box-spacer-pane% hp)

   (define show-games-help
     (show-help '("games") "About PLT Games"))

   (application-about-handler show-games-help)
   (application-preferences-handler (lambda ()
				      (message-box
				       "Oops"
				       "There aren't actually any preferences. This is just a test for Mac OS X"
				       f
				       '(ok))))
   
   (send f show #t))


				     