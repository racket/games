(module games mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "unit.ss")
	   (lib "class100.ss")
	   (lib "list.ss")
           (lib "getinfo.ss" "setup")
	   (lib "bitmap-label.ss" "mrlib")
	   "show-help.ss")

   (define game-mapping 
     (let ([games (let ([d (collection-path "games")])
                    (filter (lambda (f)
                              (let ([p (build-path d f)])
                                (and (directory-exists? p)
                                     (with-handlers ([exn:fail? (lambda (x) #f)])
                                       ((get-info (list (string->path "games") f)) 'game (lambda () #f))))))
                            (directory-list d)))])
       (map (lambda (g)
              (let ([info (get-info `(,(string->path "games") ,g))])
                (list (path->string g)
                      (info 'game (lambda () "wrong.ss"))
                      (info 'name (lambda () g))
                      (info 'game-set (lambda () "Other Games"))
		      (info 'game-icon (lambda () (build-path (collection-path "games" g)
							      (format "~a.png" g)))))))
            games)))
               
   (define f (new (class frame%
		    (augment*
		      [on-close (lambda () (exit))])
		    (super-new))
		  [label "PLT Games"]
		  [style '(metal no-resize-border)]))
   (define hp (make-object horizontal-panel% f))
   (define main (make-object vertical-panel% hp))
   (send f set-alignment 'left 'top)
   (send f stretchable-width #f)
   (send f stretchable-height #f)

   (define p (make-object horizontal-panel% main))

   (define (game-button p desc)
     (let* ([collect (car desc)]
	    [file (cadr desc)]
	    [name (caddr desc)]
	    [dir (with-handlers ([void (lambda (x) #f)])
		   (collection-path "games" collect))])
       (when dir
	 (make-object button% 
		      ((bitmap-label-maker name (list-ref desc 4))
		       p)
		      p
		      (lambda (b e)
			(let ([game-unit (dynamic-wind
                                          begin-busy-cursor
                                          (lambda () (dynamic-require (build-path dir file) 'game-unit))
                                          end-busy-cursor)])
                          (let ([c (make-custodian)])
                            (parameterize ([current-custodian c])
                              (parameterize ([current-eventspace (make-eventspace)])
                                (queue-callback
                                 (lambda ()
                                   (exit-handler (lambda (v) 
                                                   (custodian-shutdown-all c)))
                                   (invoke-unit game-unit))))))))))))
   
   (let ([game-mapping (quicksort game-mapping (lambda (a b)
						 (string<? (list-ref a 3) (list-ref b 3))))])
     (let loop ([l game-mapping])
       (unless (null? l)
	 (let* ([set (list-ref (car l) 3)]
		[p (new group-box-panel%
			[label set]
			[parent p])])
	   (let xloop ([here (list (car l))]
		       [l (cdr l)])
	     (if (and (pair? l)
		      (string=? set (list-ref (car l) 3)))
		 (xloop (cons (car l) here) (cdr l))
		 (begin
		   (for-each (lambda (g) (game-button p g)) here)
		   (loop l))))))))

   (for-each (lambda (p)
	       (let ([pred (lambda (x y) (<= (send x min-width) (send y min-width)))])
		 (send p change-children (lambda (l) (quicksort l pred)))))
	     (send p get-children))

   ; (make-object grow-box-spacer-pane% hp)

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


				     
