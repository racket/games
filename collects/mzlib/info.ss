
(lambda (request)
  (case request
    [(name) "MzLib"]
    [(compile-prefix) '(begin
			 (require-library "refer.ss")
			 (require-library "mzlibs.ss")
			 (require-library "constan.ss"))]
    [(compile-omit-files)
     (list "awk.ss" "compatm.ss" "constan.ss" "defstru.ss" 
	   "macro.ss" "macrox.ss" "match.ss"                   "macroxr.ss"
	   "letplus.ss" "letplsrc.ss" "refer.ss" "shared.ss"
	   "spidey.ss" "synrule.ss" "trace.ss" "sfunctor.ss"
	   "mzlibs.ss" "cores.ss"
	   "cmdlines.ss" "dates.ss" "strings.ss"
	   "compats.ss" "files.ss" "threads.ss"
	   "compiles.ss" "functios.ss" "pconvers.ss" "triggers.ss"
	   "inflates.ss" "prettys.ss" "zmaths.ss")]
    [else (error 'mzlib-info "Unknown request: ~s" request)]))
