; read.lsp -- OS2XLISP program uses C 5.1 CRTLIB.DLL
; Andrew Schulman 1-May-1988

; also shows new function (make-string) [entire file in one string]

(define crtlib (loadmodule "crtlib"))

(princ "File to read? ")
(define the-file (read-line))
(terpri)

(define _open (getprocaddr crtlib "_open"))
(define _filelength (getprocaddr crtlib "_filelength"))
(define _read (getprocaddr crtlib "_read"))
(define _close (getprocaddr crtlib "_close"))

(define fh (c-call _open the-file (word 0) 'word))
(if (= #xffff fh)
    (error "Can't open file"))
(format stdout "file handle = ~A\t" fh)

(define len (c-call _filelength (word fh)))
(format stdout "file length = ~A\t" len)

(define buffer (make-string #\Space len))       ; or (int-char 32) or 32

(define count (c-call _read (word fh) buffer (word len)))
(format stdout "bytes read = ~A\n\n" count)

(princ (subseq buffer 0 count))     ; entire file in string

;;; parse the file rather than just print it
; (load 'enumproc)
; (print (parse (subseq buffer 0 count) " \n\t"))

(c-call _close (word fh))

