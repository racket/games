(require-library "function.ss")

(define-struct loc (x y))
;; board = (vector-of (vector-of (union #f (make-loc n1 n2))))

;; need to make sure that the bitmap divides nicely
(define bitmap (make-object bitmap% "11.JPG"))
(define board-width 6)
(define board-height 2)

(define board
  (build-vector
   board-width
   (lambda (i)
     (build-vector
      board-height
      (lambda (j)
	(make-loc (modulo (+ i 1) board-width)
		  (modulo (+ j 1) board-height)))))))

(define (board-for-each board f)
  (let loop ([i (vector-length board)])
    (cond
     [(zero? i) (void)]
     [else
      (let ([row (vector-ref board (- i 1))])
	(let loop ([j (vector-length row)])
	  (cond
	   [(zero? j) (void)]
	   [else
	    (f (- i 1) (- j 1) (vector-ref row (- j 1)))
	    (loop (- j 1))])))
      (loop (- i 1))])))

(define (board-set! board i j v)
  (vector-set! (vector-ref board i) j v))

(define (board-get board i j)
  (vector-ref (vector-ref board i) j))

(board-set! board 0 0 #f)

(define line-brush (send the-brush-list find-or-create-brush "black" 'transparent))
(define line-pen (send the-pen-list find-or-create-pen "white" 1 'solid))
(define pict-brush (send the-brush-list find-or-create-brush "black" 'solid))
(define pict-pen (send the-pen-list find-or-create-pen "black" 1 'solid))

(define slidey-canvas%
  (class canvas% args
    (override
     [on-paint
      (lambda ()
	(board-for-each
	 board
	 (lambda (i j v)
	   (when v
	     (draw-cell v i j)))))])
    (inherit get-client-size get-dc)
    (private
      [ij->xywh
       (lambda (i j)
	 (let-values ([(w h) (get-client-size)])
	   (let ([cell-w (/ w (vector-length board))]
		 [cell-h (/ h (vector-length (vector-ref board 0)))])
	     (values (* i cell-w)
		     (* j cell-h)
		     cell-w
		     cell-h))))]
      [draw-cell
       (lambda (indicies i j)
	 (let-values ([(xd yd wd hd) (ij->xywh i j)]
		      [(xs ys ws hs) (ij->xywh (loc-x indicies)
					       (loc-y indicies))])
	   (let ([dc (get-dc)])
	     (send dc set-pen pict-pen)
	     (send dc set-brush pict-brush)
	     (send dc draw-bitmap-section bitmap xd yd xs ys wd hd)
	     (send dc set-pen line-pen)
	     (send dc set-brush line-brush)
	     (send dc draw-rectangle xd yd wd hd))))])
    (inherit stretchable-width stretchable-height min-client-width min-client-height)
    (sequence
      (apply super-init args)
      (stretchable-width #f)
      (stretchable-height #f)
      (min-client-width (send bitmap get-width))
      (min-client-height (send bitmap get-height)))))

(define f (make-object frame% "frame"))
(make-object slidey-canvas% f)
(send f show #t)