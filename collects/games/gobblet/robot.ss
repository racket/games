(module robot mzscheme
  (require (lib "unitsig.ss")
	   (lib "etc.ss")
	   (lib "list.ss")
	   "sig.ss"
	   "model.ss"
	   "explore.ss"
	   "heuristics.ss")

  (define board-size 3)
  (define steps 4)
  (define depth (if (= board-size 3) 3 2))

  (define timeout 3.0)
  (define cannon-size 128)

  (invoke-unit/sig
   (compound-unit/sig
    (import)
    (link
     [CONFIG : config^ ((unit/sig config^
			  (import)
			  (define BOARD-SIZE board-size)))]
     [MODEL : model^ (model-unit CONFIG)]
     [HEURISTICS : heuristics^ (heuristics-unit CONFIG MODEL EXPLORE)]
     [EXPLORE : explore^ (explore-unit CONFIG MODEL)]
     [ROBOT : () ((unit/sig ()
		    (import config^ explore^ model^ heuristics^)

		    (define (stack->string s)
		      (let ([s (apply string-append 
				      "...."
				      (map (lambda (p)
					     (list-ref (if (eq? 'red (piece-color p))
							   '("_" "i" "I" "|")
							   '("=" "o" "O" "0"))
						       (piece-size p)))
					   s))])
			(substring s (- (string-length s) BOARD-SIZE))))
				  
		    (define (board->string depth b)
		      (let jloop ([j 0])
			(if (= j BOARD-SIZE)
			    ""
			    (string-append
			     (make-string depth #\space)
			     (let iloop ([i 0])
			       (if (= i BOARD-SIZE)
				   ""
				   (string-append (stack->string (board-ref b i j))
						  " "
						  (iloop (add1 i)))))
			     "\n"
			     (jloop (add1 j))))))

		    ;; Play-a-game test
		    (let ([search (make-search)])
		      (let loop ([board empty-board]
				 [who 'red]
				 [who-moved "no one"])
			(cond
			 [(winner? board who)
			  (printf "~a wins!~n~a~n" who (board->string 1 board))]
			 [(winner? board (other who))
			  (printf "~a wins!~n~a~n" (other who) (board->string 1 board))]
			 [else
			  (printf "~n~a moved; ~a's turn~n~a~n" who-moved who (board->string 1 board))
			  (let ([start (current-inexact-milliseconds)]
				[m (search timeout steps depth cannon-size 
					   (if (= BOARD-SIZE 3)
					       make-3x3-rate-board
					       make-4x4-rate-board)
					   (if (= BOARD-SIZE 3)
					       make-3x3-canned-moves
					       make-4x4-canned-moves)
					   who board)])
			    (printf "[~a secs]~n" (/ (- (current-inexact-milliseconds) start)
						     1000.0))
			    (loop (apply-play board m) (other who) who))]))))
		  CONFIG EXPLORE MODEL HEURISTICS)])
    (export))))
