(module gobblet mzscheme
  (require (lib "unitsig.ss")
	   (lib "unit.ss")
	   (lib "file.ss")
	   (lib "mred.ss" "mred")
	   "sig.ss"
	   "model.ss"
	   "gui.ss")

  (provide game-unit)

  (define (make-gobblet-unit size)
    (compound-unit/sig
     (import)
     (link [CONFIG : config^ ((unit/sig config^
				(import)
				(define BOARD-SIZE size)))]
	   [RESTART : restart^ ((unit/sig restart^
				  (import)
				  (define (new-game n)
				    (put-preferences '(gobblet:board-size) (list n))
				    (parameterize ([current-eventspace orig-eventspace])
				      (queue-callback
				       (lambda ()
					 (start-gobblet n)))))))]
	   [MODEL : model^ (model-unit CONFIG)]
	   [GUI : () (gui-unit CONFIG MODEL RESTART)])
     (export)))

  (define orig-eventspace (current-eventspace))
  
  (define (start-gobblet board-size)
    ;; Start a new game as a child process:
    (parameterize ([current-custodian (make-custodian)])
      (parameterize ([exit-handler (lambda (v)
				     (custodian-shutdown-all (current-custodian)))])
	(parameterize ([current-eventspace (make-eventspace)])
	  (queue-callback
	   (lambda ()
	     (invoke-unit/sig (make-gobblet-unit board-size))))))))

  (define game-unit 
    (unit
      (import)
      (export)
      (start-gobblet (get-preference 'gobblet:board-size (lambda () 3))))))
