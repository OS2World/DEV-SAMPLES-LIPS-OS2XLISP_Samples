; help.lsp
; run-time dynamic linking to Microsoft QHELP

; topic
(if (not (boundp 'topic))
    (define topic "List categories"))

(if (not (define help (loadmodule "mshelp")))
    (error "This program requires MSHELP.DLL"))
        
(define help.open (getprocaddr help "HELPOPEN"))
(define help.nc (getprocaddr help "HELPNC"))
(define help.look (getprocaddr help "HELPLOOK"))
(define help.decomp (getprocaddr help "HELPDECOMP"))
(define help.getline (getprocaddr help "HELPGETLINE"))
(define help.getcells (getprocaddr help "HELPGETCELLS"))

(define strseg (dos-alloc-seg 0))       ; 64K
(define str (mk-fp strseg 0))

; should scan environment for QH variable

(define context (call help.open "d:\\os2\\qh\\qh.hlp" 'long))
(define request (call help.nc topic context 'long))
(define len (call help.look request str))
(call help.decomp str (+ 32000 str) request)

(define buf (make-string 32 len))
(define categories nil)
(define i 0)
(while (> (call help.getline (word (+ i 2)) (word len) buf (+ 32000 str)) 1)
    (format stdout "~A\n" buf)
    (incr i))

; categories now contains list of categories

(dos-free-seg strseg)
(freemodule help)

; to get actual help text:  HelpGetCells(short usLines, 76, buf, decompbuf,
; attr-array[5]=0x07,0x70,0x02,0x0F,0x30)   ????
; increment usLines
