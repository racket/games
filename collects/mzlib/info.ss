
(let ([elaboration-time-files
       (list "awk.ss" "compatm.ss" "constan.ss" "defstru.ss" 
	     "macro.ss" "macrox.ss" "match.ss"
	     "shared.ss" "restarts.ss" "cmdlinem.ss"
	     "spidey.ss" "synrule.ss" "trace.ss"
	     "cmdlines.ss" "dates.ss" "strings.ss"
	     "compats.ss" "files.ss" "threads.ss"
	     "compiles.ss" "functios.ss" "pconvers.ss"
	     "inflates.ss" "prettys.ss" "zmaths.ss"
	     "cores.ss"
	     "mzlibs.ss")])
  (lambda (request failure)
    (case request
      [(name) "MzLib"]
      [(compile-prefix) '(begin
			   (require-library "refer.ss")
			   (require-library "mzlibs.ss")
			   (require-library "constan.ss"))]
      [(compile-omit-files) (append elaboration-time-files
				    (list "refer.ss" "letplsrc.ss" "sfunctor.ss"))]
      [(compile-elaboration-zos) elaboration-time-files]
      [else (failure)])))
