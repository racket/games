(invoke-unit/sig
 (unit/sig ()
   (import mred^)

   (define frame (make-object frame% "Lights Out"))
   
   (define-struct board (name board))
   
   (define boards
     (map (lambda (x) (apply make-board x))
          (require-library "boards.ss" "games" "lights-out")))
   
   (define (new-board)
     (letrec ([dialog (make-object dialog% "New Board")]
              [mode 'prebuilt]
              [update-mode
               (lambda ()
                 (send below-panel change-children
                       (case mode
                         [(random)
                          (lambda x (list random-panel))]
                         [(prebuilt)
                          (lambda x (list prebuilt-panel))]
                         [(empty)
                          (lambda x (list random-panel))])))]
              [radio-box
               (make-object radio-box% #f (list "Prebuilt" "Random" "Empty") dialog
                 (lambda (rb evt)
                   (cond
                     [(= 0 (send rb get-selection))
                      (set! mode 'prebuilt)]
                     [(= 1 (send rb get-selection))
                      (set! mode 'random)]
                     [(= 2 (send rb get-selection))
                      (set! mode 'empty)])
                   (update-mode))
                 '(horizontal))]
              [below-panel (make-object vertical-panel% dialog)]
              [prebuilt-panel (make-object vertical-panel% below-panel '(border))]
              [prebuilt
               (make-object choice%
                 #f
                 (map board-name boards)
                 prebuilt-panel
                 (lambda (choice evt)
                   (void)))]
              [random-panel (make-object vertical-panel% below-panel '(border))]
              [random-slider
               (make-object slider%
                 "Board Size" 3 8 random-panel
                 (lambda (slider evt)
                   (void))
                 6)]
              [button-panel (make-object horizontal-panel% dialog)]
              [cancel? #t]
              [ok (make-object button% "OK" 
                    button-panel
                    (lambda x
                      (set! cancel? #f)
                      (send dialog show #f)))]
              [cancel (make-object button% "Cancel"
                        button-panel
                        (lambda x
                          (send dialog show #f)))])
       (update-mode)
       (send button-panel set-alignment 'right 'center)
       (set! new-board
             (lambda ()
               (set! cancel? #t)
               (send dialog show #t)
               (if cancel?
                   #f
                   (case mode
                     [(random)
                      (build-random-board (send random-slider get-value))]
                     [(empty)
                      (map
                       vector->list
                       (vector->list
                        (build-vector 
                         (send random-slider get-value)
                         (lambda (x) (make-vector (send random-slider get-value) 'o)))))]
                     [(prebuilt)
                      (board-board (list-ref boards (send prebuilt get-selection)))]))))
       (new-board)))
              
   (define (build-vector n f)
     (list->vector
      (let loop ([n n])
        (cond
          [(zero? n) null]
          [else (cons (f (- n 1)) (loop (- n 1)))]))))
   
   (define (build-random-board n)
     (let* ([choices
             (let loop ([i n]
                        [res null])
               (cond
                 [(zero? i) res]
                 [else
                  (loop (- i 1)
                        (let loop ([j n]
                                   [res res])
                          (cond
                            [(zero? j) res]
                            [else (loop (- j 1)
                                        (cons (cons (- i 1) (- j 1)) res))])))]))]
            [board (build-vector n (lambda (x) (make-vector n 'o)))]
            [flip
             (lambda (i j)
               (when (and (<= 0 i (- n 1))
                          (<= 0 j (- n 1)))
                 (vector-set! (vector-ref board i) j 
                              (case (vector-ref (vector-ref board i) j)
                                [(x) 'o]
                                [(o) 'x]))))]
            [sim-click
             (lambda (i j)
               (flip i j)
               (flip (- i 1) j)
               (flip (+ i 1) j)
               (flip i (+ j 1))
               (flip i (- j 1)))]
            
            [number-of-clicks
             (let loop ([n (* (+ n 1) 2)])
               (cond
                 [(zero? n) 0]
                 [else (+ (random 2)
                          (loop (- n 1)))]))])
                    
       (let loop ([clicks number-of-clicks])
         (unless (zero? clicks)
           (let ([choice (random (length choices))]
                 [continue? (not (zero? (random 3)))]
                 [choice-coordinates #f])
             (set! choices
                   (let loop ([choices choices]
                              [n choice])
                     (cond
                       [(zero? n) 
                        ;(printf "choose: ~a~n" (car choices))
                        (set! choice-coordinates (car choices))
                        (cdr choices)]
                       [else (cons (car choices) (loop (cdr choices) (- n 1)))])))
             (sim-click (car choice-coordinates)
                        (cdr choice-coordinates))
             (loop (- clicks 1)))))
       (map vector->list (vector->list board))))
   
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
   
   (define current-board #f)
   
   (define (init-board new-board)
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
        (let loop ([n (length new-board)]
                   [initss new-board])
          (cond
            [(null? initss) null]
            [else (cons (list->vector (make-row (- (length new-board) n) (car initss)))
                        (loop (- n 1) (cdr initss)))]))))
     (send main-buttons-panel change-children (lambda (x) null))
     (set! current-board new-board)
     (set! buttons (make-buttons)))
   
   (define button-panel (make-object horizontal-panel% frame))
   
   (make-object button% "New" button-panel
     (lambda x
       (let ([res (new-board)])
         (when res
           (init-board res)))))
   
   (make-object button% "Reset" button-panel
     (lambda x
       (init-board current-board)))
   
   (let ([help ((require-library "show-help.ss" "games")
                (list "games" "lights-out")
                "Lights Out Help")])
     (make-object button% "Help" button-panel
       (lambda x
         (help))))
   
   (make-object grow-box-spacer-pane% button-panel)

   (init-board (build-random-board (+ 3 
                                      (random 2)
                                      (random 2)
                                      (random 2)
                                      (random 2)
                                      (random 2))))
   (send frame stretchable-width #f)
   (send frame stretchable-height #f)
   (send frame show #t))
 mred^)