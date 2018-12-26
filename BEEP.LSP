; beep.lsp -- for OS2XLISP
; Andrew Schulman 25-April-1988

; illustrates:
;   calling OS/2 functions (DOSBEEP and VIOWRTCHARSTRATT)
;   named parameters
;   what happens when you write sample programs at 3am

(define dosbeep (getprocaddr doscalls "DOSBEEP"))
(define viowrtcharstratt (getprocaddr viocalls "VIOWRTCHARSTRATT"))

(define (dos-beep freq dur)
    (call dosbeep (word freq) (word dur)))
        
(define (vio-wrt msg row col attr)
    (call viowrtcharstratt msg
        (word (length msg)) (makelong row col) (addr (word attr)) (word 0)))

; problem with preceding is that have to remember order:  does frequency
; come before duration?.  Does col before row, or after?  What about msg?
; Who the hell can remember!  Would prefer to have named parameters, that can
; be entered in any arbitrary order (cf. Jon Bentley, "More Programming
; Pearls," chapter on self-describing data).  Lisp makes this easy with
; the &key keyword.  Also, the parameters have default values, though these
; are not always that useful!

(define (dos-beep &key (freq 1000) (dur 100))
    (call dosbeep (word freq) (word dur)))
        
(define (vio-wrt &key (msg "I'll drink to that") (row 10) (col 10) (attr 50))
    (call viowrtcharstratt msg
        (word (length msg)) (makelong row col) (addr (word attr)) (word 0)))

; the following three calls are identical (and not just because they're
; all commented out!)

; (dos-beep :freq 1000 :dur 100)
; (dos-beep :dur 100 :freq 1000)        ; different order from definition
; (dos-beep)                            ; use default values

; (cls) is a function defined in the file INIT.LSP
(cls)

(vio-wrt :row 3 :col 0 :attr 5
    :msg "This is supposed to sound like a computer in a '50s movie")
(vio-wrt :row 4 :col 0 :attr 5 :msg "Do not fold, spindle, or mutilate!")   

(dotimes
    (i 35)
    (define x (random 2000))
    (vio-wrt :msg (format nil "~A" x) :row 10 :col 10 :attr 15)
    (dos-beep :dur 200 :freq x))
        
; the one problem with these named parameters is that if a function is
; set up to use them, then you HAVE to use them (unless you go for the
; defaults)

(set-cursor 24 0)

