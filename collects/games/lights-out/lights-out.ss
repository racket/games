(invoke-unit/sig
 (unit/sig ()
   (import mred^)
   (define frame (make-object frame% "Lights Out"))
   
   (define-struct board (name board))
   
   (define boards
     (list
      (make-board
       "Empty"
       '((o o o o o)
         (o o o o o)
         (o o o o o)
         (o o o o o)
         (o o o o o)))
      (make-board
       "1"
       '((o o o o o)
         (o o o o o)
         (x o x o x)
         (o o o o o)
         (o o o o o)))
      (make-board
       "2"
       '((x o x o x)
         (x o x o x)
         (o o o o o)
         (x o x o x)
         (x o x o x)))
      (make-board
       "3"
       '((o x o x o)
         (x x o x x)
         (x x o x x)
         (x x o x x)
         (o x o x o)))
      (make-board
       "4"
       '((o o o o o)
         (x x o x x)
         (o o o o o)
         (x o o o x)
         (x x o x x)))
      (make-board
       "5"
       '((x x x x o)
         (x x x o x)
         (x x x o x)
         (o o o x x)
         (x x o x x)))
      (make-board
       "6"
       '((o o o o o)
         (o o o o o)
         (x o x o x)
         (x o x o x)
         (o x x x o)))
      (make-board
       "7"
       '((x x x x o)
         (x o o o x)
         (x o o o x)
         (x o o o x)
         (x x x x o)))
      (make-board 
       "Diagonal"
       '((o o o o x)
         (o o o x o)
         (o o x o o)
         (o x o o o)
         (x o o o o)))))
   
   (make-object choice%
     #f
     (map board-name boards)
     frame
     (lambda (choice evt)
       (init-board (board-board (list-ref boards (send choice get-selection))))))
   
   (define main-buttons-panel (make-object vertical-panel% frame))
   
   (define label-size 30)
   
   (define on-label (make-object bitmap% label-size label-size))
   (define off-label (make-object bitmap% label-size label-size))
   
   (let ()
     (define bdc (make-object bitmap-dc%))
     (define (f b c)
       (send bdc set-bitmap b)
       (send bdc set-pen (send the-pen-list find-or-create-pen c 1 'solid))
       (send bdc set-brush (send the-brush-list find-or-create-brush c 'solid))
       (send bdc draw-rectangle 0 0 label-size label-size)
       (send bdc set-bitmap #F))
     
     (f on-label "green")
     (f off-label "white"))
   
   (define toggle-button%
     (class button% (label panel callback)
       (inherit set-label)
       (private
         [state (eq? label 'x)])
       (public
         [toggle
          (lambda ()
            (set! state (not state))
            (set-label (if state on-label off-label)))])
       (sequence
         (super-init (if state on-label off-label) panel callback))))
   
   (define (toggle-button i j size)
     (when (and (<= 0 i (- size 1))
                (<= 0 j (- size 1)))
       (send (vector-ref (vector-ref buttons i) j) toggle)))
   
   (define (button-callback i j size)
     (lambda xxx
       (toggle-button i j size)
       (toggle-button (- i 1) j size)
       (toggle-button i (- j 1) size)
       (toggle-button (+ i 1) j size)
       (toggle-button i (+ j 1) size)))
   
   (define buttons 'uninitialized)
   
   (define (init-board board)
     (define (make-buttons)
       (define (make-row i inits)
         (let ([hp (make-object horizontal-panel% main-buttons-panel)]
               [size (length inits)])
           (let loop ([n (length inits)]
                      [inits inits])
             (cond
               [(null? inits) null]
               [else (cons (make-object toggle-button% (car inits) hp 
                             (button-callback 
                              i (- size n)
                              size))
                           (loop (- n 1)
                                 (cdr inits)))]))))
       (list->vector
        (let loop ([n (length board)]
                   [initss board])
          (cond
            [(null? initss) null]
            [else (cons (list->vector (make-row (- (length board) n) (car initss)))
                        (loop (- n 1) (cdr initss)))]))))
     (send main-buttons-panel change-children (lambda (x) null))
     (set! buttons (make-buttons)))
   
   (define button-panel (make-object horizontal-panel% frame))
   
   (let ([help ((require-library "show-help.ss" "games")
                (list "games" "lights-out")
                "Lights Out Help")])
     (make-object button% "Help" button-panel
       (lambda x
         (help))))
   
   (make-object grow-box-spacer-pane% button-panel)
   
   (init-board (board-board (car boards)))
   (send frame show #t))
 mred^)