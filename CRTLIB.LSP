; crtlib.lsp -- run-time dynamic linking to C 5.1 CRTLIB.DLL
; for OS2XLISP
; Andrew Schulman 1-May-1988

(if (define crtlib (loadmodule "crtlib"))
	(princ "Run-time dynamic linking to CRTLIB.DLL\n")
	(error "This program requires CRTLIB.DLL (from C 5.1)"))
		
(define crtlib.malloc (getprocaddr crtlib "_malloc"))
(define crtlib.free (getprocaddr crtlib "_free"))
(define crtlib.memcpy (getprocaddr crtlib "_memcpy"))
(define crtlib.memset (getprocaddr crtlib "_memset"))
(define crtlib.strlen (getprocaddr crtlib "_strlen"))
(define crtlib.strcpy (getprocaddr crtlib "_strcpy"))
(define crtlib.strstr (getprocaddr crtlib "_strstr"))
(define crtlib.ultoa (getprocaddr crtlib "_ultoa"))
(define crtlib.tolower (getprocaddr crtlib "_tolower"))
(define crtlib.getcwd (getprocaddr crtlib "_getcwd"))
(define crtlib.remove (getprocaddr crtlib "_remove"))

(define (malloc i) (c-call crtlib.malloc i 'ptr))
(define (free p) (c-call crtlib.free p))
(define (memcpy s t i) (c-call crtlib.memcpy s t i 'ptr))
(define (memset s t i) (c-call crtlib.memset s t i 'ptr))
(define (strcpy s t) (c-call crtlib.strcpy s t 'str))
(define (strstr s t) (c-call crtlib.strstr s t 'ptr))
(define (ultoa v s r) (c-call crtlib.ultoa v s r 'str))
(define (tolower c) (c-call crtlib.tolower c))
(define (strlen s) (c-call crtlib.strlen s))
(define (pwd buf) (c-call crtlib.getcwd buf (length buf) 'str))
(define (remove f) (c-call crtlib.remove f))

;;; NOTE THAT ALL C CALLS BELOW ARE "TRANSPARENT"
		
(define ptr (malloc 50))
(define msg "this is a test of memcpy")
(memcpy ptr msg (1+ (strlen msg)))	; copy the zero as well
(format stdout "After memcpy: ptr is \"~A\"\n" (peek ptr 0))

(define ptr2 (malloc 50))
(strcpy ptr2 "this is a test of strcpy")
(format stdout "After strcpy: ptr2 is \"~A\"\n" (peek ptr2 0))

(define cmp (strstr ptr2 "test"))
(format stdout "strstr returns \"~A\": index ~A\n"
	(peek cmp 0) (- cmp ptr2))
		
(define ptr3 (malloc 30))
(memset ptr3 (word 32) (word 29))
(poke (+ ptr3 29) 0)		; make ASCIIZ string 
(format stdout "After memset: ptr3 is \"~A\"\n" (peek ptr3 0))

(ultoa #xfefe ptr3 (word 10))
(format stdout "After ultoa: #xfefe is ~A " (peek ptr3 0))
(ultoa #xfefe ptr3 (word 2))
(format stdout "(binary ~A)\n" (peek ptr3 0))

(format stdout "strlen(\"~A\") is ~A\n" "hello" (strlen "hello"))

(define *word-format* nil)

(format stdout "tolower(~A) is ~A\n" (int-char 65) (int-char (tolower 65)))

(define buf (make-string (int-char 32) 128))
(format stdout "PWD: ~A\n" (pwd buf))

(free ptr)
(free ptr2)
(free ptr3)

(freemodule crtlib)
