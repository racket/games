(module gui mzscheme
  (require (lib "gl-board.ss" "games" "gl-board-game")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "gl-vectors.ss" "sgl")
           (prefix gl- (lib "sgl.ss" "sgl"))
	   (lib "unitsig.ss")
	   "sig.ss")

  (provide gui-unit)
  
  (define gui-unit
    (unit/sig ()
      (import config^ model^ restart^)

      ;; Configuration ------------------------------

      (define JR? (= BOARD-SIZE 3))
      
      (define PIECE-SIZES (if JR?
			      '(0.4 0.6 0.75)
			      '(0.3 0.45 0.65 0.8)))
      
      ;; GUI ------------------------------
      
      (define yellow (gl-float-vector 1.0 1.0 0.0 1.0))
      (define red (gl-float-vector 1.0 0.0 0.0 1.0))
      (define light-blue (gl-float-vector 0.5 0.5 1.0 1.0))
      (define dark-blue (gl-float-vector 0.0 0.0 1.0 1.0))
      
      ;; A gui-piece is
      ;;  (make-gui-piece piece gl-description num num)
      ;;  where the nums might be < 0 or > BOARD-SIZE
      (define-struct gui-piece (piece dl i j))

      ;; State ------------------------------

      ;; The state of the game, as reflected in the GUI:
      (define board empty-board)
      (define turn 'red)

      ;; past, future : (list-of (cons thunk thunk))
      ;;                where first thunk is do and second is undo
      (define past null)
      (define future null)

      ;; When `playing?' is true, double-check reset request
      (define playing? #f)

      ;; GUI Move ------------------------------

      ;; This function is called when the user tries to move `gp'
      ;;  to location `to'
      (define (gui-move gp to)
	(when (gui-piece? gp)
	  ;; Get dest and source locations:
	  (let* ((to-i (inexact->exact (floor (gl-vector-ref to 0))))
		 (to-j (inexact->exact (floor (gl-vector-ref to 1))))
		 (from-i (gui-piece-i gp))
		 (from-j (gui-piece-j gp))
		 (on-board? (<= 0 from-i (sub1 BOARD-SIZE))))
	    ;; Only move if the requent lands on the board:
	    (when (and (<= 0 to-i (sub1 BOARD-SIZE))
		       (<= 0 to-j (sub1 BOARD-SIZE)))
	      ;; Only move if the model says that it's ok:
	      (move board (gui-piece-piece gp) 
		    (and on-board? from-i) (and on-board? from-j)
		    to-i to-j
		    (lambda (new-board)
		      ;; Move allowed by the model. Create a thunk to
		      ;; execute this move and a thunk to undo this
		      ;; move:
		      (let ([new-gp (make-gui-piece (gui-piece-piece gp) (gui-piece-dl gp)
						    to-i to-j)]
			    [old-board board]
			    [old-turn turn])
			(action!
			 ;; Forward thunk:
			 (lambda ()
			   (set! board new-board)
			   (send gui-board remove-piece gp)
			   (gui-add-piece new-gp)
			   (let ([r? (winner? new-board 'red)]
				 [y? (winner? new-board 'yellow)])
			     (cond
			      [(and r? y?) (set-winner! (case old-turn
							  [(red) "Yellow"]
							  [(yellow) "Red"]))]
			      [r? (set-winner! "Red")]
			      [y? (set-winner! "Yellow")]
			      [else (set-turn! (other old-turn))])))
			 ;; Rewind thunk:
			 (lambda ()
			   (set! board old-board)
			   (send gui-board remove-piece new-gp)
			   (gui-add-piece gp)
			   (set-turn! old-turn)))))
		    (lambda ()
		      ;; Move not allowed by model
		      (void)))))))

      ;; GUI Board and Pieces ------------------------------

      (define f (new frame% (label "Gobblet") (width 800) (height 600)))
      (define gui-board
	(new gl-board% (parent f) 
	     (min-x (if JR? (- 1 BOARD-SIZE) -1)) (max-x (if JR? (sub1 (* 2 BOARD-SIZE)) (add1 BOARD-SIZE)))
	     (min-y 0) (max-y BOARD-SIZE)
	     (lift 1.2)
	     (move gui-move)))

      (define q
	(send gui-board with-gl-context
	      (lambda () (gl-new-quadric))))

      ;; Space description:
      (define space-dl
	(send gui-board with-gl-context
	      (lambda ()
		(let ((list-id (gl-gen-lists 1)))
		  (gl-quadric-draw-style q 'fill)
		  (gl-quadric-normals q 'smooth)
		  (gl-new-list list-id 'compile)
		  (gl-material-v 'front 'ambient-and-diffuse dark-blue)
		  (gl-begin 'polygon)
		  (gl-vertex 0.0 0.0 -0.02)
		  (gl-vertex 1.0 0.0 -0.02)
		  (gl-vertex 1.0 1.0 -0.02)
		  (gl-vertex 0.0 1.0 -0.02)
		  (gl-end)
		  (gl-material-v 'front 'ambient-and-diffuse light-blue)
		  (gl-push-matrix)
		  (gl-translate 0.5 0.5 -0.01)
		  (gl-disk q 0.0 .40 25 1)
		  (gl-pop-matrix)
		  (gl-end-list)
		  list-id))))

      ;; Install spaces on board:
      (fold-board (lambda (i j v)
		    (send gui-board add-space
			  (lambda ()
			    (gl-push-matrix)
			    (gl-translate i j 0.01)
			    (gl-call-list space-dl)
			    (gl-pop-matrix))
			  (cons i j)))
		  void)
      
      ;; Piece description-maker:
      (define (make-piece-dl color scale)
	(send gui-board with-gl-context
	      (lambda ()
		(let ((list-id (gl-gen-lists 1)))
		  (gl-quadric-draw-style q 'fill)
		  (gl-quadric-normals q 'smooth)
		  (gl-new-list list-id 'compile)
		  (gl-material-v 'front 'ambient-and-diffuse color)
		  (gl-cylinder q (/ scale 2) (/ scale 2) (* 1.5 scale) 25 1)
		  (gl-push-matrix)
		  (gl-translate 0.0 0.0 (* 1.5 scale))
		  (gl-disk q 0.0 (/ scale 2) 25 1)
		  (gl-pop-matrix)
		  (gl-end-list)
		  list-id))))

      ;; Red piece descriptions:
      (define red-dls (map (lambda (size)
			     (make-piece-dl red size))
			   PIECE-SIZES))

      ;; Yellow piece descriptions:
      (define yellow-dls (map (lambda (size)
				(make-piece-dl yellow size))
			      PIECE-SIZES))

      ;; GUI piece records, with each piece at its initial place:
      (define gui-pieces
	(let loop ([red-dls red-dls][yellow-dls yellow-dls]
		   [red-pieces red-pieces][yellow-pieces yellow-pieces]
		   [sizes PIECE-SIZES][z 0])
	  (if (null? red-dls)
	      null
	      (append
	       (let ([sz (car sizes)])
		 (let loop ([dw (if JR?
				    (- BOARD-SIZE 2)
				    (- BOARD-SIZE 1.5))])
		   (if (negative? dw)
		       null
		       (list*
			(make-gui-piece (car red-pieces) (car red-dls) 
					(if JR? (- dw BOARD-SIZE -1) -1)
					(if JR? z dw))
			(make-gui-piece (car yellow-pieces) (car yellow-dls) 
					(if JR? (+ BOARD-SIZE dw) BOARD-SIZE)
					(if JR? z dw))
			(loop (sub1 dw))))))
	       (loop (cdr red-dls) (cdr yellow-dls) 
		     (cdr red-pieces) (cdr yellow-pieces) 
		     (cdr sizes) (+ z 1))))))
      
      ;; Places a gui-piece at its location on the board:
      (define (gui-add-piece gp)
	(send gui-board add-piece 
	      (+ (gui-piece-i gp) 0.5) (+ (gui-piece-j gp) 0.5) 0
	      (lambda () (gl-call-list (gui-piece-dl gp)))
	      gp))

      ;; Extra GUI controls ----------------------------------------

      ;; Define a 3-element pane that makes the left and right parts
      ;;  the same width (so that the middle part is centered):
      (define bottom (new (class horizontal-pane% 
			    ;; Override place-children for the 3-child case,
			    ;;  make first and third the same width
			    (define/override (place-children l w h)
			      (let ([r (super place-children l w h)])
				(if (= (length r) 3)
				    (let ([a (list-ref r 0)]
					  [b (list-ref r 1)]
					  [c (list-ref r 2)])
				      (let* ([aw (list-ref a 2)]
					     [cw (list-ref c 2)]
					     [naw (quotient (+ aw cw) 2)])
					(list
					 (list (car a) (cadr a) naw (cadddr a))
					 (list (+ (car b) (- naw aw)) (cadr b) (caddr b) (cadddr b))
					 (list (+ naw (caddr b)) (cadr c) (- (+ cw aw) naw) (cadddr c)))))
				    r)))
			    (super-new))
			  (parent f)
			  (stretchable-height #f)))

      ;; Status message:
      (define msg
	(new message% (label "") (parent bottom) (stretchable-width #t)))
      
      ;; Forward & Reverse buttons
      (define bottom-middle (new horizontal-pane% 
				 (parent bottom)
				 (stretchable-height #f)
				 (stretchable-width #f)))
      (define arrows? (let ([f (make-object font% 12 'system)])
			(and (send f screen-glyph-exists? #\u25C0)
			     (send f screen-glyph-exists? #\u25B6))))
      (define backward-button
	(new button% (label (if arrows? " \u25C0 " " < ")) (parent bottom-middle) 
	     (callback (lambda (b e) (backward!)))))
      (define forward-button
	(new button% (label (if arrows? " \u25B6 " " > ")) (parent bottom-middle)
	     (callback (lambda (b e) (forward!)))))
      (define (enable-buttons)
	(send backward-button enable (pair? past))
	(send forward-button enable (pair? future)))

      ;; Reset & Help buttons:
      (define bottom-right (new horizontal-pane% 
				(parent bottom)
				(stretchable-height #f)
				(alignment '(right center))))
      (new button% (label "Reset") (parent bottom-right)
	   (callback (lambda (b e)
		       (when (or (not playing?)
				 (equal? 1 (message-box/custom
					    "Warning"
					    "Stop game in progress and reset?"
					    "Reset"
					    "Cancel"
					    #f
					    f
					    '(default=1 caution))))
			 (reset!)))))
      (new button% (label (if (= BOARD-SIZE 3) "4x4 Game" "3x3 Game"))
	   (parent bottom-right)
	   (callback (lambda (b e)
		       (new-game (if (= BOARD-SIZE 3) 4 3)))))
      (new button% (label "Help") (parent bottom-right)
	   (callback (lambda (b e)
		       (show-gobblet-help))))

      ;; Extra controls ----------------------------------------
      
      (define (action! forward backward)
	(set! playing? #t)
	(set! future null)
	(set! past (cons (cons forward backward) past))
	(forward)
	(enable-buttons))

      (define (backward!)
	(let ([fb (car past)])
	  (set! past (cdr past))
	  (set! future (cons fb future))
	  ((cdr fb))
	  (enable-buttons)
	  (send gui-board refresh)))

      (define (forward!)
	(let ([fb (car future)])
	  (set! future (cdr future))
	  (set! past (cons fb past))
	  ((car fb))
	  (enable-buttons)
	  (send gui-board refresh)))

      (define (reset!)
	(for-each (lambda (p)
		    (send gui-board remove-piece p))
		  (send gui-board get-pieces))
	(init-game!)
	(send gui-board refresh))

      (define (set-turn! c)
	(set! turn c)
	(send msg set-label (format "~a's turn" (if (eq? turn 'red) "Red" "Yellow")))
	(for-each (lambda (p)
		    (send gui-board enable-piece p (eq? c (piece-color (gui-piece-piece p)))))
		  (send gui-board get-pieces)))

      (define (set-winner! who)
	(set! playing? #f)
	(send msg set-label (format "~a wins!" who))
	(for-each (lambda (p)
		    (send gui-board enable-piece p #f))
		  (send gui-board get-pieces)))

      (define (init-game!)
	(set! board empty-board)
	(set! past null)
	(set! future null)
	(set! playing? #f)
	(enable-buttons)
	(for-each gui-add-piece gui-pieces)
	(set-turn! 'red))

      ;; Go ----------------------------------------
      
      (init-game!)

      (send f show #t))))
