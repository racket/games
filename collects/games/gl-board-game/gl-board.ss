(module gl-board mzscheme
  (require (prefix gl- (lib "sgl.ss" "sgl"))
           (lib "gl.ss" "sgl")
           (lib "gl-vectors.ss" "sgl")
           (lib "class.ss")
           (lib "list.ss")
           (lib "mred.ss" "mred"))
  
  (provide gl-board%)

  (define-struct space (draw info))
  (define-struct piece (x y z draw info))
  
  (define (get-info x)
    (cond
      ((piece? x) (piece-info x))
      ((space? x) (space-info x))
      (else #f)))
  
  ;; interpolate : real gl-double-vector gl-double-vector -> gl-double-vector
  ;; returns the point on the p1-p2 line with the given z coordinate.
  (define (interpolate z p1 p2)
    (let* ((v (gl-double-vector- p2 p1))
           (c (/ (- z (gl-vector-ref p1 2))
                 (gl-vector-ref v 2))))
      (gl-double-vector+ p1 (gl-double-vector* c v))))
  
  (define (get-viewport)
    (glGetIntegerv GL_VIEWPORT 4))

  (define (get-projection)
    (glGetDoublev GL_PROJECTION_MATRIX 16))
    
  (define (get-modelview)
    (glGetDoublev GL_MODELVIEW_MATRIX 16))

  (define gl-board%
    (class canvas%
      (inherit with-gl-context swap-gl-buffers refresh get-width get-height)
      
      ;; min-x, max-x, min-y, max-y, lift: real
      ;; move: info gl-double-vector ->
      ;; min-x, max-x, min-y and max-y specify the dimensions of the board.
      ;; lift specifies how far a piece is picked up when being moved.
      ;; move is called when a piece is moved to a space (possibly it's current space),
      ;; when a space is clicked on, and when a space is dragged to another space.
      ;; move is given the info of the piece or space selected, and coordinates
      ;; it is moved to.
      (init-field min-x max-x min-y max-y lift (move void))
      
      (define spaces null)
      (define pieces null)
      
      ;; add-space: (->) info ->
      ;; Adds a space to this board.  The draw thunk should draw the space 
      ;; when called.  The value of the info argument will be given to
      ;; the move function when the space is selected.
      (define/public (add-space draw info)
        (set! spaces (cons (make-space draw info) spaces)))
      
      ;; add-piece: real real real (->) info ->
      ;; Adds a piece to this board.  The draw thunk should draw the piece
      ;; when called.  The value of the info argument will be given to
      ;; the move function when the piece is selected.  The piece is translated
      ;; by the x, y and z arguments before drawing.
      (define/public (add-piece x y z draw info)
        (set! pieces (cons (make-piece x y z draw info) pieces)))
      
      ;; remove-piece: info ->
      ;; Removes all pieces whose info is equal? to p-i from this board.
      (define/public (remove-piece p-i)
        (set! pieces (filter
                      (lambda (x)
                        (not (equal? p-i (piece-info x))))
                      pieces)))
      
      ;; How far the light is from the board's center
      (define light-distance (* 4.0 (max (- max-x min-x) (- max-y min-y))))
      ;; The board's center
      (define center-x (/ (+ max-x min-x) 2))
      (define center-y (/ (+ max-y min-y) 2))

      (define eye-distance (* 2.0 (max (- max-x min-x) (- max-y min-y))))
      (define delta-eye-distance (/ eye-distance 20.0))
      (define fov 30)
      (define theta 45)
      (define phi 0)
      
      ;; Transformation used to draw shadows.
      (define shadow-projection 
        (let ((ld (- light-distance (* .0001 eye-distance))))
          (gl-double-vector ld 0 0 0
                            0 ld 0 0 
                            0 0 ld 0
                            0 0 (- 1) 0)))
      
      ;; Either #f or the currently selected piece.
      (define mouse-state #f)
      
      ;; dragging : (union #f 3-element-gl-double-vector)
      ;; The mouse's location while dragging
      (define dragging #f)
      
      ;; draw : bool bool bool ->
      ;; Draws the scene.  If select? is true, then names are loaded for selection
      ;; with each space getting named after its index in the spaces list, and 
      ;; each piece by its index plus the number of spaces.
      (define/private (draw select? spaces? pieces?)
        (when spaces?
          (gl-normal 0.0 0.0 1.0)
          (let loop ((i 0)
                     (s spaces))
            (unless (null? s)
              (when select?
                (gl-load-name i))
              ((space-draw (car s)))
              (loop (add1 i)
                    (cdr s)))))
        (when pieces?
          (let loop ((i (length spaces))
                     (ps (if (and (piece? mouse-state) dragging)
                             (cons (make-piece (gl-vector-ref dragging 0)
                                               (gl-vector-ref dragging 1)
                                               (gl-vector-ref dragging 2)
                                               (piece-draw mouse-state)
                                               (piece-info mouse-state))
                                   pieces)
                             pieces)))
            (unless (null? ps)
              (let ((p (car ps)))
                (unless (and dragging (eq? mouse-state p))  ;; Don't draw the dragged piece
                                                            ;; in its home location.
                  (when select?
                    (gl-load-name i))
                  (gl-push-matrix)
                  (gl-translate (piece-x p) (piece-y p) (piece-z p))
                  ((piece-draw p))
                  (gl-pop-matrix))
                (loop (add1 i)
                      (cdr ps)))))))
      
      (define/override (on-paint)
        (with-gl-context
         (lambda ()
           (gl-clear 'color-buffer-bit 'depth-buffer-bit)
           (draw #f #t #t)
           ;; Very simple shadowing on the board
           (gl-push-matrix)
           (gl-translate center-x center-y light-distance)
           (gl-mult-transpose-matrix shadow-projection)
           (gl-translate (- center-x) (- center-y) (- light-distance))
           (gl-disable 'lighting)
           (gl-color 0 0 0)
           (draw #f #f #t)
           (gl-enable 'lighting)
           (gl-pop-matrix)
           (gl-flush)
           (swap-gl-buffers))))
      
      (define/override (on-size w h)
        (with-gl-context
         (lambda ()
           (gl-viewport 0 0 w h)
           (setup-view/proj)))
        (refresh))
      
      (define/private (setup-view/proj)
        (gl-matrix-mode 'projection)
        (gl-load-identity)
        (gl-perspective fov (/ (get-width) (get-height))
                        (/ eye-distance 2) (* 2 eye-distance))
        (gl-matrix-mode 'modelview)
        (gl-load-identity)
        (gl-translate 0 0 (- eye-distance))
        (gl-rotate (- theta) 1 0 0)
        (gl-rotate phi 0 0 1)
        (gl-translate (- center-x) (- center-y) 0))
      
      ;; pick: real real -> (union piece space #f)
      ;; Returns the piece or space at screen coordinates x y and #f is there
      ;; is no such object.
      (define/private (pick x y)
        (let ((vp (get-viewport))
              (proj (get-projection))
              (selection (glSelectBuffer 512)))
          (gl-render-mode 'select)
          (gl-matrix-mode 'projection)
          (gl-push-matrix)
          (gl-load-identity)
          (gl-pick-matrix x (- (gl-vector-ref vp 3) y 1) 1.0 1.0 vp)
          (gl-mult-matrix proj)
          (gl-matrix-mode 'modelview)
          (gl-init-names)
          (gl-push-name 0)
          (draw #t #t #t)
          (gl-matrix-mode 'projection)
          (gl-pop-matrix)
          (gl-matrix-mode 'modelview)
          (gl-flush)
          (let* ((hits (gl-render-mode 'render))
                 (results (mergesort (gl-process-selection (select-buffer->gl-uint-vector selection)
                                                           hits)
                                     (lambda (a b)
                                       (< (gl-selection-record-min-z a)
                                          (gl-selection-record-min-z b))))))
            (cond
              ((null? results) #f)
              (else
               (let ((index (car (gl-selection-record-stack (car results)))))
                 (cond
                   ((< index (length spaces))
                    (list-ref spaces index))
                   (else
                    (list-ref pieces (- index (length spaces)))))))))))
         
               
      ;; screen-world: real real real -> gl-double-vector
      ;; Given a screen x and y, return the world x, y, z 
      ;; corresponding to the given world z.
      (define/private (screen->world x y z)
        (let* ((v (get-viewport))
               (m (get-modelview))
               (p (get-projection))
               (real-y (- (gl-vector-ref v 3) y 1)))
          (interpolate z
                       (gl-un-project x real-y 0.0 m p v)
                       (gl-un-project x real-y 1.0 m p v))))
                                      
      (define/override (on-event e)
        (cond
          ((send e button-down? 'left)
           (with-gl-context
            (lambda ()
              (set! mouse-state (pick (send e get-x) (send e get-y)))
              (set! dragging (screen->world (send e get-x) (send e get-y) lift))))
           (refresh))
          ((and mouse-state
                (or (send e button-up? 'left)
                    (and (send e moving?) (not (and (send e dragging?) (send e get-left-down))))))
           (move (get-info mouse-state)
                 (if dragging
                     dragging
                     (with-gl-context 
                      (lambda ()
                        (screen->world (send e get-x) (send e get-y) 0.0)))))
           (set! mouse-state #f)
           (set! dragging #f)
           (refresh))
          ((and (send e dragging?) (send e get-left-down))
           (with-gl-context
            (lambda ()
              (set! dragging (screen->world (send e get-x) (send e get-y) lift))))
           (refresh))))
      
      (define/override (on-char e)
        (case (send e get-key-code)
          ((#\a) (set! phi (+ phi 5)))
          ((#\d) (set! phi (- phi 5)))
          ((#\w) (set! theta (- theta 5)))
          ((#\s) (set! theta (+ theta 5)))
          ((left) (unless (< fov 10)
                    (set! fov (- fov 5))))
          ((right) (unless (> fov 170)
                     (set! fov (+ fov 5))))
          ((up) (set! eye-distance (- eye-distance 5)))
          ((down) (set! eye-distance (+ eye-distance 5))))
        (with-gl-context
         (lambda ()
           (setup-view/proj)))
        (refresh))
      
      (let ([cfg (new gl-config%)])
	(send cfg set-multisample-size 4)
	(super-new (style '(no-autoclear)) (gl-config cfg)))
      
      ;; initial setup
      (with-gl-context
       (lambda ()
         (gl-shade-model 'smooth)
         (gl-enable 'lighting)
         (gl-enable 'light0)
         (gl-enable 'depth-test)
         ;(gl-light-model 'light-model-local-viewer 1.0)
         ;(gl-light-model-v 'light-model-ambient (gl-float-vector 0.2 0.2 0.2 1.0))
         (gl-light-v 'light0 'position (gl-float-vector center-x center-y light-distance 1.0))
         ;(gl-light-v 'light0 'ambient (gl-float-vector 0.0 0.0 0.0 1.0))
         ;(gl-light-v 'light0 'diffuse (gl-float-vector 0.0 0.0 0.0 1.0))
         ;(gl-light-v 'light0 'specular (gl-float-vector 0.0 0.0 0.0 1.0))
         (gl-clear-color 1.0 1.0 1.0 1.0)
         (setup-view/proj)))))
  )
