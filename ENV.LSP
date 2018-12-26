; env.lsp
; Andrew Schulman 17-Feb-1988
; revised 15-Mar-1988

(define dosgetenv (getprocaddr doscalls "DOSGETENV"))
(define envseg 0)
(define cmdline 0)
(call dosgetenv (addr envseg) (addr cmdline))
(format stdout "Envseg ~A  Cmdline ~A~%~%" envseg cmdline)          

(define (print-strings seg off)
    (do
        ((len 1) (s ""))                            ; init
        ((= 0 len))                                 ; test
        (setf s (peek (mk-fp seg off) 0))           ; body
        (if
            (>
                (setf len (length (setf s (peek (mk-fp seg off) 0))))
                0)
            (format stdout "~A~%" s))
        (setf off (+ 1 len off))))

(print-strings envseg cmdline)

(terpri)

(print-strings envseg 0)

(terpri)

;;; note: individual values can be retrieved with DOSSCANENV
;;; DOSSCANENV located in DOSCALL1.DLL
(define (env-test)
    (let
        ((dosscanenv (register "DOSCALL1" "DOSSCANENV"))
         (env-ptr 0))
    (princ "This is a test of the DOSSCANENV function\n")
    (princ "Please enter the name of an environment variable: ")
    (define var-name (read))
    (if 
        (zerop 
            (call dosscanenv (addr (symbol-name var-name)) (addr env-ptr)))
        ; if okay
        (set var-name (peek env-ptr 0))
        ; if not okay
        (set var-name nil))
    (format stdout
        "Created XLISP variable ~A with value \"~A\"~%" var-name
            (eval var-name))))
        
(env-test)
