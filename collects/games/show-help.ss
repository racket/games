(lambda (collections frame-title)
  (let* ([f #f]
	 [f%
	  (class frame% ()
	    (override
	     [on-close
	      (lambda ()
		(set! f #f))])
	    (sequence
	      (super-init frame-title)))])
    (lambda ()
      (if f
	  (send f show #t)
	  (let* ([frame (make-object f%)]
		 [t (make-object text%)]
		 [c (make-object editor-canvas% frame t)])
	    (send c min-width 500)
	    (send c min-height 300)
	    (send t auto-wrap #t)
	    (call-with-input-file (build-path
				   (apply collection-path collections)
				   "doc.txt")
	      (lambda (p)
		(let loop ()
		  (let ([l (read-line p)])
		    (unless (eof-object? l)
		      (cond
		       [(string=? l "")
			(send t insert #\newline)
			(send t insert #\newline)]
		       [else
			(send t insert l)
			(let ([last-char (string-ref l (- (string-length l) 1))])
			  (unless (char=? #\space last-char)
			    (send t insert #\space)))])
		      (loop)))))
	      'text)
	    (send t set-position 0 0)
	    (send frame show #t)
	    (set! f frame))))))
