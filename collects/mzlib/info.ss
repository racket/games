
(let ([elaboration-time-files
       (list "awk.ss" "compatm.ss" "defstru.ss" 
	     "macro.ss" "macrox.ss" "match.ss"
	     "shared.ss" "restarts.ss" "cmdlinem.ss"
	     "spidey.ss" "synrule.ss" "trace.ss"
	     "cmdlines.ss" "dates.ss" "strings.ss"
	     "compats.ss" "files.ss" "threads.ss" "transcrs.ss"
	     "compiles.ss" "functios.ss" "pconvers.ss"
	     "inflates.ss" "prettys.ss" "maths.ss"
	     "cores.ss" "coreflats.ss"
	     "mzlibs.ss" "mzlibflats.ss")])
  (lambda (request failure)
    (case request
      [(name) "MzLib"]
      [(compile-prefix) '(begin
			   (require-library "refer.ss")
			   (require-library "mzlibs.ss"))]
      [(compile-omit-files) (append elaboration-time-files
				    (list "refer.ss" "letplsrc.ss"))]
      [(compile-elaboration-zos) elaboration-time-files]
      [else (failure)])))
