(module checkers mzscheme
  (require (lib "gl-board.ss" "games" "gl-board-game")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "gl-vectors.ss" "sgl")
           (prefix gl- (lib "sgl.ss" "sgl"))
           (lib "gl.ss" "sgl")
           (lib "array.ss" "srfi" "25")
           (lib "unit.ss")
           (rename (lib "gl-frame.ss" "sgl" "examples")
                   image->gl-vector image->gl-vector))
  
  (provide game-unit)

  (define dim-red (gl-float-vector .8 0.0 0.0 1.0))
  (define gray (gl-float-vector 0.2 0.2 0.2 1.0))

  (define light-img (image->gl-vector "light.jpg"))
  (define dark-img (image->gl-vector "dark.jpg"))
  
  (define-struct space-info (x y))
  (define-struct piece-info (x y color king?) (make-inspector))
  (define-struct moves (list forced-jump?))

  (define checkers-view@
    (unit
      (import move)
      (export add-space add-piece remove-piece move-piece set-turn show)

      (define (add-space space color)
        (let ((list-id (if (eq? color 'light) light-square dark-square)))
          (send board add-space
                (lambda ()
                  (gl-push-matrix)
                  (gl-translate (space-info-x space) (space-info-y space) 0)
                  (gl-call-list list-id)
                  (gl-pop-matrix))
                space)))
      
      (define (get-piece-draw-fn piece glow?)
        (let ((list-id (get-piece-dl (piece-info-color piece)
                                     (piece-info-king? piece))))
          (if glow?
              (lambda ()
                (gl-material-v 'front 'emission (gl-float-vector 0.1 0.1 0.1 1.0))
                (gl-call-list list-id)
                (gl-material-v 'front 'emission (gl-float-vector 0.0 0.0 0.0 1.0)))
              (lambda () 
                (gl-call-list list-id)))))
      
      (define add-piece
        (case-lambda
          ((piece) (add-piece piece #f))
          ((piece glow?)
           (send board add-piece (+ .5 (piece-info-x piece)) (+ .5 (piece-info-y piece)) 0.0
                 (get-piece-draw-fn piece glow?)
                 piece))))
          
      (define (move-piece from to-x to-y)
        (remove-piece from)
        (add-piece (make-piece-info to-x to-y (piece-info-color from) (piece-info-king? from))))
      
      (define (remove-piece p)
        (send board remove-piece p))

      (define (internal-move old move-to)
        (when (piece-info? old)
          (move old move-to)))

      (define (set-turn turn moves)
	(let* ([pieces (send board get-pieces)])
	  (for-each (lambda (p)
                      (send board set-piece-draw p
                            (get-piece-draw-fn p #f))
                      (send board enable-piece p #f))
		    pieces)
	  (for-each (lambda (p)
                      (send board set-piece-draw p
                            (get-piece-draw-fn p #t))
                      (send board enable-piece p #t))
                    (moves-list moves)))
	(send msg set-label
	      (if (null? (moves-list moves))
		  (format "~a wins!" (if (eq? turn 'red) "Black" "Red"))
		  (format "~a's turn~a" 
			  (if (eq? turn 'red) "Red" "Black")
			  (if (moves-forced-jump? moves)
			      " - must take jump"
			      "")))))
      
      (define f (new frame% (label "Checkers") (width 800) (height 600)))
      (define board
        (new gl-board% (parent f) (min-x 0.0) (max-x 8.0) (min-y 0.0) (max-y 8.0)
             (lift .35)
             (move internal-move)))
      (define msg
	(new message% (label "") (parent f) (stretchable-width #t)))
      
      (define q
        (send board with-gl-context
          (lambda () (gl-new-quadric))))
      
      (define (make-piece-dl real-color height)
        (send board with-gl-context
          (lambda ()
            (let ((list-id (gl-gen-lists 1)))
              (gl-quadric-draw-style q 'fill)
              (gl-quadric-normals q 'smooth)
              (gl-new-list list-id 'compile)
              ;(gl-material-v 'front 'specular (gl-float-vector 1.0 1.0 1.0 1.0))
              ;(gl-material 'front 'shininess 120.0)  
              (gl-material-v 'front 'ambient-and-diffuse real-color)
              (gl-cylinder q .35 .35 height 25 1)
              (gl-push-matrix)
              (gl-translate 0.0 0.0 height)
              (gl-disk q 0.0 .35 25 1)
              (gl-pop-matrix)
              (gl-end-list)
              list-id))))

      (define red-piece (make-piece-dl dim-red .2))
      (define red-king (make-piece-dl dim-red .4))
      (define black-piece (make-piece-dl gray .2))
      (define black-king (make-piece-dl gray .4))

      (define (get-piece-dl color king?)
        (cond
          ((eq? color 'red)
           (if king? red-king red-piece))
          (else
           (if king? black-king black-piece))))
      
      (define-values (dark-tex light-tex)
        (send board with-gl-context
          (lambda ()
            (let ((x (glGenTextures 2)))
              (values
               (gl-vector-ref x 0)
               (gl-vector-ref x 1))))))

      (define (init-tex tex img)
        (send board with-gl-context
          (lambda ()
            (glBindTexture GL_TEXTURE_2D tex)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
            (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
            (glTexImage2D GL_TEXTURE_2D 0 GL_RGB (car img) (cadr img) 0
                          GL_RGB GL_UNSIGNED_BYTE (caddr img)))))
      
      (init-tex light-tex light-img)
      (init-tex dark-tex dark-img)

      (define (make-square-dl color)
        (send board with-gl-context
          (lambda ()
            (let ((list-id (gl-gen-lists 1)))
              (gl-new-list list-id 'compile)
              (gl-enable 'texture-2d)
              (glBindTexture GL_TEXTURE_2D color)
              (gl-material-v 'front 'ambient-and-diffuse
                             (gl-float-vector 1 1 1 1))
              (gl-begin 'polygon)
              (gl-tex-coord 0.0 0.0)
              (gl-vertex 0.0 0.0 0.0)
              (gl-tex-coord 1.0 0.0)
              (gl-vertex 1.0 0.0 0.0)
              (gl-tex-coord 1.0 1.0)
              (gl-vertex 1.0 1.0 0.0)
              (gl-tex-coord 0.0 1.0)
              (gl-vertex 0.0 1.0 0.0)
              (gl-end)
              (gl-disable 'texture-2d)
              (gl-end-list)
              list-id))))
      
      (define dark-square (make-square-dl dark-tex))
      (define light-square (make-square-dl light-tex))
      
      (define (show)
        (send f show #t))))
  
  (define checkers-model@
    (unit
      (import add-space add-piece remove-piece move-piece set-turn)
      (export move)

      (define turn 'red)
      (define board (make-array (shape 0 8 0 8) #f))
      
      (let loop ((i 0)
                 (j 0))
        (cond
          ((and (< j 8) (< i 8))
           (cond
             ((even? (+ i j))
              (add-space (make-space-info j i) 'dark)
              (cond
                ((< i 3)
                 (array-set! board j i (cons 'red #f))
                 (add-piece (make-piece-info j i 'red #f)))
                ((> i 4)
                 (array-set! board j i (cons 'black #f))
                 (add-piece (make-piece-info j i 'black #f)))))
             (else
              (add-space (make-space-info j i) 'light)))
           (loop i (add1 j)))
          ((< i 8) (loop (add1 i) 0))))

      (define (other-color c)
        (cond
          ((eq? c 'red) 'black)
          (else 'red)))
      
      (define (single-move-ok? direction from-x from-y to-x to-y)
        (and (= to-y (+ direction from-y))
             (= 1 (abs (- from-x to-x)))))

      (define (can-move? direction from-x from-y)
	(and (<= 0 (+ from-y direction) 7)
	     (or (and (<= 0 (+ from-x 1) 7)
		      (not (array-ref board (+ from-x 1) (+ from-y direction))))
		 (and (<= 0 (+ from-x -1) 7)
		      (not (array-ref board (+ from-x -1) (+ from-y direction)))))))
      
      (define (get-jumped-piece color direction from-x from-y to-x to-y)
        (and (= to-y (+ direction direction from-y))
             (= 2 (abs (- from-x to-x)))
             (let* ((jumped-x (+ from-x (/ (- to-x from-x) 2)))
                    (jumped-y (+ from-y direction))
                    (jumped-piece (array-ref board jumped-x jumped-y)))
               (and jumped-piece
                    (eq? (other-color color) (car jumped-piece))
                    (make-piece-info jumped-x jumped-y (car jumped-piece) (cdr jumped-piece))))))
      
      (define (can-jump? direction from-color from-x from-y)
        (let ((to-y (+ direction direction from-y))
              (to-x1 (+ from-x 2))
              (to-x2 (- from-x 2)))
          (and (<= 0 to-y 7)
               (or (and (<= 0 to-x1 7)
                        (not (array-ref board to-x1 to-y))
                        (get-jumped-piece from-color direction
                                          from-x from-y
                                          to-x1 to-y))
                   (and (<= 0 to-x2)
                        (not (array-ref board to-x2 to-y))
                        (get-jumped-piece from-color direction
                                          from-x from-y
                                          to-x2 to-y))))))
      

      (define (fold-board f v)
	(let iloop ([i 0][v v])
	  (if (= i 8)
	      v
	      (let jloop ([j 0][v v])
		(if (= j 8)
		    (iloop (add1 i) v)
		    (jloop (add1 j) 
			   (if (even? (+ i j)) 
			       (f i j v)
			       v))))))) 

      (define (get-jump-moves)
	(let ([direction (if (eq? turn 'red) 1 -1)])
	  (fold-board
	   (lambda (i j l)
	     (let ([p (array-ref board i j)])
	       (if (and p
			(eq? (car p) turn)
			(or (can-jump? direction turn i j)
			    (and (cdr p)
				 (can-jump? (- direction) turn i j))))
		   (cons (make-piece-info i j turn (cdr p)) l)
		   l)))
	   null)))

      (define (get-moves)
	(let ([jumps (get-jump-moves)])
	  (if (pair? jumps)
	      (make-moves jumps #t)
	      (make-moves
	       (let ([direction (if (eq? turn 'red) 1 -1)])
		 (fold-board
		  (lambda (i j l)
		    (let ([p (array-ref board i j)])
		      (if (and p
			       (eq? (car p) turn)
			       (or (can-move? direction i j)
				   (and (cdr p)
					(can-move? (- direction) i j))))
			  (cons (make-piece-info i j turn (cdr p)) l)
			  l)))
		  null))
	       #f))))

      (define (move from to)
        (let* ((to-x (inexact->exact (floor (gl-vector-ref to 0))))
               (to-y (inexact->exact (floor (gl-vector-ref to 1))))
               (from-x (piece-info-x from))
               (from-y (piece-info-y from))
               (from-color (piece-info-color from))
               (from-king? (piece-info-king? from))
               (to-king? (or from-king?
                             (if (eq? 'red from-color)
                                 (= to-y 7)
                                 (= to-y 0))))
               (direction (if (eq? turn 'red) 1 -1)))
          (when (and (eq? turn from-color)
                     (<= 0 to-x 7)
                     (<= 0 to-y 7)
                     (not (array-ref board to-x to-y)))
            (cond
              ((and (null? (get-jump-moves))
		    (or (single-move-ok? direction from-x from-y to-x to-y)
			(and from-king?
			     (single-move-ok? (- direction) from-x from-y to-x to-y))))
	       (move-piece from to-x to-y)
               (set! turn (other-color from-color))
               (array-set! board to-x to-y (cons from-color to-king?))
               (array-set! board from-x from-y #f)
               (when (and to-king? (not from-king?))
                 (remove-piece (make-piece-info to-x to-y from-color from-king?))
                 (add-piece (make-piece-info to-x to-y from-color to-king?)))
	       (set-turn turn (get-moves)))
              ((or (get-jumped-piece from-color direction from-x from-y to-x to-y)
		   (and from-king?
			(get-jumped-piece from-color (- direction) from-x from-y to-x to-y)))
               =>
               (lambda (j)
                 (remove-piece j)
                 (move-piece from to-x to-y)
                 (array-set! board (piece-info-x j) (piece-info-y j) #f)
                 (array-set! board from-x from-y #f)
                 (array-set! board to-x to-y (cons from-color to-king?))
                 (when (and to-king? (not from-king?))
                   (remove-piece (make-piece-info to-x to-y from-color from-king?))
                   (add-piece (make-piece-info to-x to-y from-color to-king?)))
                 (cond
                   ((or (can-jump? direction from-color to-x to-y)
                        (and from-king?
                             (can-jump? (- direction) from-color to-x to-y)))
                    (set-turn turn (make-moves (list (make-piece-info to-x to-y from-color to-king?)) #t)))
                   (else
                    (set! turn (other-color from-color))
                    (set-turn turn (get-moves))))))))))

      (set-turn turn (get-moves))
      ))
              
  (define game-unit
   (compound-unit 
     (import)
     (link 
      (VIEW (checkers-view@ (MODEL move)))
      (MODEL (checkers-model@ (VIEW add-space add-piece remove-piece move-piece set-turn)))
      (SHOW ((unit (import show) (export) (show)) (VIEW show))))
     (export)))
  )
