(module checkers mzscheme
  (require (lib "gl-board.ss" "games" "gl-board-game")
           (lib "class.ss")
           (lib "mred.ss" "mred")
           (lib "gl-vectors.ss" "sgl")
           (prefix gl- (lib "sgl.ss" "sgl"))
           (lib "array.ss" "srfi" "25")
           (lib "unit.ss"))
  
  (define red (gl-float-vector 1.0 0.0 0.0 1.0))
  (define dim-red (gl-float-vector .8 0.0 0.0 1.0))
  (define gray (gl-float-vector 0.2 0.2 0.2 1.0))
  (define black (gl-float-vector 0.0 0.0 0.0 1.0))
  
  (define-struct space-info (x y))
  (define-struct piece-info (x y color king?) (make-inspector))

  (define checkers-view@
    (unit
      (import move)
      (export add-space add-piece remove-piece move-piece set-turn show jump-seq)

  
      (define (add-space space color)
        (send board with-gl-context
          (lambda ()
            (let ((list-id (gl-gen-lists 1)))
              (gl-new-list list-id 'compile)
              (gl-material-v 'front 'ambient-and-diffuse color)
              (gl-push-matrix)
              (gl-translate (space-info-x space) (space-info-y space) 0)
              (gl-begin 'polygon)
              (gl-vertex 0.0 0.0 0.0)
              (gl-vertex 1.0 0.0 0.0)
              (gl-vertex 1.0 1.0 0.0)
              (gl-vertex 0.0 1.0 0.0)
              (gl-end)
              (gl-pop-matrix)
              (gl-end-list)
              (send board add-space (lambda () (gl-call-list list-id)) space)))))
          
      (define add-piece
        (case-lambda
          ((piece) (add-piece piece #f))
          ((piece glow?)
           (send board with-gl-context
             (lambda ()
               (let ((list-id (gl-gen-lists 1))
                     (height (if (piece-info-king? piece) .4 .2))
                     (real-color (if (eq? (piece-info-color piece) 'red) dim-red gray)))
                 (gl-quadric-draw-style q 'fill)
                 (gl-quadric-normals q 'smooth)
                 (gl-new-list list-id 'compile)
                 (when glow?
                   (gl-material-v 'front 'emission (gl-float-vector 0.2 0.2 0.2 1.0)))
                 ;(gl-material-v 'front 'specular (gl-float-vector 1.0 1.0 1.0 1.0))
                 ;(gl-material 'front 'shininess 120.0)  
                 (gl-material-v 'front 'ambient-and-diffuse real-color)
                 (gl-cylinder q .35 .35 height 15 1)
                 (gl-push-matrix)
                 (gl-translate 0.0 0.0 height)
                 (gl-disk q 0.0 .35 15 1)
                 (when glow?
                   (gl-material-v 'front 'emission (gl-float-vector 0.0 0.0 0.0 1.0)))
                 (gl-pop-matrix)
                 (gl-end-list)
                 (send board add-piece (+ .5 (piece-info-x piece)) (+ .5 (piece-info-y piece)) 0.0
                       (lambda () (gl-call-list list-id))
                       piece)))))))
          
      (define (move-piece from to-x to-y)
        (remove-piece from)
        (add-piece (make-piece-info to-x to-y (piece-info-color from) (piece-info-king? from))))
      
      (define (remove-piece p)
        (send board remove-piece p))

      (define (internal-move old move-to)
        (when (piece-info? old)
          (move old move-to)))
      
      (define (set-turn turn)
        (send tf set-value (symbol->string turn)))
      
      (define (jump-seq p)
        (remove-piece p)
        (add-piece p #t))
      
      (define f (new frame% (label "gl-checkers") (width 800) (height 600)))
      (define vp (new vertical-pane% (parent f)))
      (define tf (new text-field% (parent vp) (label "turn") (enabled #f) (init-value "red")
                      (callback void)))
      (define board
        (new gl-board% (parent vp) (min-x 0.0) (max-x 8.0) (min-y 0.0) (max-y 8.0)
             (lift .35)
             (move internal-move)))
      
      (define q
        (send board with-gl-context
          (lambda () (gl-new-quadric))))
      
      (define (show)
        (send f show #t))))
  
  (define checkers-model@
    (unit
      (import add-space add-piece remove-piece move-piece set-turn jump-seq)
      (export move)

      (define turn 'red)
      (define must-jump #f)
      (define board (make-array (shape 0 8 0 8) #f))
      
      (let loop ((i 0)
                 (j 0))
        (cond
          ((and (< j 8) (< i 8))
           (cond
             ((even? (+ i j))
              (add-space (make-space-info j i) black)
              (cond
                ((< i 3)
                 (array-set! board j i (cons 'red #f))
                 (add-piece (make-piece-info j i 'red #f)))
                ((> i 4)
                 (array-set! board j i (cons 'black #f))
                 (add-piece (make-piece-info j i 'black #f)))))
             (else
              (add-space (make-space-info j i) red)))
           (loop i (add1 j)))
          ((< i 8) (loop (add1 i) 0))))

      (define (other-color c)
        (cond
          ((eq? c 'red) 'black)
          (else 'red)))
      
      (define (single-move-ok? direction from-x from-y to-x to-y)
        (and (= to-y (+ direction from-y))
             (= 1 (abs (- from-x to-x)))))
      
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
      
      (define (move from to)
        (let* ((to-x (inexact->exact (floor (gl-double-vector-ref to 0))))
               (to-y (inexact->exact (floor (gl-double-vector-ref to 1))))
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
              ((and (not must-jump)
                    (or (single-move-ok? direction from-x from-y to-x to-y)
                        (and from-king?
                             (single-move-ok? (- direction) from-x from-y to-x to-y))))
               (move-piece from to-x to-y)
               (set! turn (other-color from-color))
               (set-turn turn)
               (array-set! board to-x to-y (cons from-color to-king?))
               (array-set! board from-x from-y #f)
               (when (and to-king? (not from-king?))
                 (remove-piece (make-piece-info to-x to-y from-color from-king?))
                 (add-piece (make-piece-info to-x to-y from-color to-king?))))
              ((and (or (not must-jump)
                        (and (= from-x (car must-jump))
                             (= from-y (cdr must-jump))))
                    (or (get-jumped-piece from-color direction from-x from-y to-x to-y)
                        (and from-king?
                             (get-jumped-piece from-color (- direction) from-x from-y to-x to-y))))
               =>
               (lambda (j)
                 (remove-piece j)
                 (move-piece from to-x to-y)
                 (array-set! board (piece-info-x j) (piece-info-y j) #f)
                 (array-set! board from-x from-y #f)
                 (array-set! board to-x to-y (cons from-color from-king?))
                 (when (and to-king? (not from-king?))
                   (remove-piece (make-piece-info to-x to-y from-color from-king?))
                   (add-piece (make-piece-info to-x to-y from-color to-king?)))
                 (cond
                   ((or (can-jump? direction from-color to-x to-y)
                        (and to-king?
                             (can-jump? (- direction) from-color to-x to-y)))
                    (jump-seq (make-piece-info to-x to-y from-color to-king?))
                    (set! must-jump (cons to-x to-y)))
                   (else
                    (set! turn (other-color from-color))
                    (set-turn turn)
                    (set! must-jump #f)))))))))
      ))
              
  (define-values/invoke-unit (show)
   (compound-unit 
     (import)
     (link 
      (VIEW (checkers-view@ (MODEL move)))
      (MODEL (checkers-model@ (VIEW add-space add-piece remove-piece move-piece set-turn jump-seq))))
     (export (VIEW show))))
  (show)
  )
