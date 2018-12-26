; huge.lsp
; allocate huge memory in OS/2
; for OS2XLISP v. 1.5
; Andrew Schulman 27-June-1988

(define DOSALLOCHUGE (getprocaddr doscalls "DOSALLOCHUGE"))
(define DOSGETHUGESHIFT (getprocaddr doscalls "DOSGETHUGESHIFT"))
(define DOSFREESEG (getprocaddr doscalls "DOSFREESEG"))

(define (alloc-huge segs
         &optional (rem 0) (opt 0)              ; optional parameters
         &aux (seg 0) (shift 0) (lst nil))      ; local variables
    (if (not (zerop 
        (call DOSALLOCHUGE (word segs) (word rem) ^seg (word 0) (word opt))))
        nil
        ; else
        (progn
            (call DOSGETHUGESHIFT ^shift)
            (setf shift (shl 1 shift))
            (setf lst (list (new-node seg)))    ; (new-node) needed!!
            (dotimes 
                (i (if (zerop rem) (1- segs) segs))
                (nconc lst (list (+ seg (* (1+ i) shift)))))
            lst)))
            
(define (free-huge seg)
    (zerop (call DOSFREESEG (word (if (listp seg) (car seg) seg)))))


