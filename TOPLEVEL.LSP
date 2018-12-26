; toplevel.lsp -- OS2XLISP
; Andrew Schulman 27-April-1988
; uses KBDSTRINGIN and Estes's ALIAS program

; make sure they have ALIAS replacement for KBDSTRINGIN
(if (not (define tmp (loadmodule "alias")))
    (error "TOPLEVEL is not really useable without ALIAS.DLL")
    (freemodule tmp))

; set up for KbdStringIn call       
(define KBDSTRINGIN (getprocaddr kbdcalls "KBDSTRINGIN"))
(define length (makelong 255 0))        ; 2-word structure
(define buf (make-string (int-char 32) 255))

; could hold off all ^C and ^break with DOSHOLDSIGNAL

(define (my-read-line)
    (call KBDSTRINGIN buf (addr length) (word 0) (word 0))
    (terpri)
    (subseq buf 0 (shr length 16)))     ; trim string

(define (get-expr)
    (do
        ((s "") (op 1) (cl 0))              ; init
        ((>= cl op) s)                      ; test, retval
        (setf s (strcat s " " (my-read-line)))
        (setf op (char-count s #\())
        (setf cl (char-count s #\)))))
            
(define (read-eval-print)
    (print (eval (read (make-string-input-stream (get-expr))))))
        
(define (do-prompt)
    (if (eq 'STRING (type-of *prompt*))
        (princ *prompt*)
    ; else
        (eval *prompt*)))
            
; catch any errors
(define *breakenable* nil)

; THE LOOP          
(while t
    (do-prompt)
    (read-eval-print))
        
; lines are now editable with cursor keys
; backward-scrollable semi-history

; problem! tabs not expanded! (modify source?)

