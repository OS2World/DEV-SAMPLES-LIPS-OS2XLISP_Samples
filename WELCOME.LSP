; welcome.lsp
; Andrew Schulman 24-Feb-1988 (revised 15-Mar-88)
; (revised 17-Apr-88:  VIOPOPUP)
; for OS/2 XLISP

; get the addresses of the OS/2 functions
(define vio-wrt-char-str-att (getprocaddr viocalls "VIOWRTCHARSTRATT"))
(define vio-scroll-dn (getprocaddr viocalls "VIOSCROLLDN"))
(define kbd-char-in (getprocaddr kbdcalls "KBDCHARIN"))
            
(define w0 (word 0))            
            
;;; an example of higher-level access to the OS/2 functions
(define (vio-wrt msg row col attr)
    (call
        vio-wrt-char-str-att
        msg                             ; same as (addr msg)
        (word (length msg))
        (makelong row col)
        (addr (word attr))
        w0))

(define (wait)
    (call
        kbd-char-in
        (make-string 32 10)                  ; we're ignoring input data
        w0                                   ; wait for the character
        w0))

;;; clear an area of the screen
(define (clear top left bottom right)
    (call
        vio-scroll-dn
        (word top) (word left)
        (word bottom) (word right)
        (word -1)
        (addr (word 32))
        w0))

;;; "sprintf" some data into a string
(define (whoami)
    (format nil "PID ~A, THREAD ~A, GROUP ~A"
        (getpid)
        (thread-id)
        (getgrp)))
            
(define (who-is-foreground)
    (format nil "Foreground PID ~A, Foreground Group ~A"
        (foreground-pid)
        (foreground-session)))
            
;;; make string of duplicated characters -- uses (repeat) macro in INIT.LSP
;;; this is a little slow now; to make (border) faster, it should be optimized
;;; cf. Steele, p.302
;(define (make-string n ch)
;   (let
;       ((str1 "") (str2 (string (int-char ch))))
;       (repeat n 
;           (define str1
;               (strcat str1 str2)))
;       str1))
; forget preceding - now an OS2XLISP built-in
            
;;; make a border
(define (border top left bottom right)
    (let ((tmp *word-format*))
        (define *word-format* nil)          
        (clear top left bottom right)
        (vio-wrt
            (format nil "~A~A~A"
                (int-char 201)              ; top left
                (make-string
                    (int-char 205)          ; top row
                    (- right left 1))       ; note triple subtraction
                (int-char 187))             ; top right
            top left 10)
        (let
            ((e (string (int-char 186))))
            (dotimes
                (i (- bottom top 1))
                (vio-wrt e (+ top 1 i) left 10)     ; left edge
                (vio-wrt e (+ top 1 i) right 10)))  ; right edge
        (vio-wrt
            (format nil "~A~A~A"
                (int-char 200)                  ; bottom left
                (make-string
                    (int-char 205)              ; bottom row
                    (- right left 1))
                (int-char 188))                 ; bottom right
            bottom left 10)
        (define *word-format* tmp)))
        
; clear the screen
(clear 0 0 25 80)
;(border 0 0 24 79)

(dotimes
    (i 400)
    (vio-wrt "Welcome to OS/2 XLISP!" (rem i 24) (random 79) i))
(vio-wrt (whoami) 14 25 10)     
        
; make a little pop-up
; note that this operates in a different screen group, and will pop up
; over whatever is on the screen, even if OS2XLISP is running in the
; background

;;; NOTE! if somehow welcome.lsp breaks between the VIOPOPUP and the
;;; VIOENDPOPUP, you won't be able to switch away.  To release the popup,
;;; just type at the OS2XLISP prompt:
;;; > (call (getprocaddr viocalls "VIOENDPOPUP") w0)

(call 
    (getprocaddr viocalls "VIOPOPUP")
    (addr (word 3))
    w0)
(border 3 20 12 56)
(vio-wrt "         OS2XLISP Pop-up        " 4 22 10)
(vio-wrt "    Press any key to continue   " 6 22 10)
(vio-wrt "Note that the speed isn't so bad" 8 22 10)
(vio-wrt "even though this is interpreted." 9 22 10)
(vio-wrt (whoami) 11 25 10)

; wait for a keystroke, restore the screen, then wait a sec
(wait)
(call
    (getprocaddr viocalls "VIOENDPOPUP")        ; this operation is very fast!
    w0)
(call 
    (getprocaddr doscalls "DOSSLEEP")           ; sleep a second
    1000)

;;; 
;;; ANSI FUNCTIONS
;;;
(define (rev s)
    (format stdout "\033[7m~A\033[0m~%" s))
        
(define (set-cursor row col)
    (format stdout "\033[~A;~AH" row col))

(define (cls)
    (princ "\033[2J")
    nil)
        
;;;
;;; SCREEN SET-UP
;;;
(cls)
(rev "Runtime Dynamic Linking with OS2XLISP\n")

(define (help)
    '(date time elapsed-time vers foreground-session foreground-pid
    boot-drive getpid getppid getgrp priority thread-id subsession
    foreground? protect-only? present? code? dos-alloc-seg dos-free-seg
    register dos-mem-avail cls))

;;;
;;; DISPLAY
;;;
(define (pr p) (format stdout "~A~%" p))

(pr "Now that DOSGETINFOSEG has been called, the following new XLISP")
(pr "functions can be called.  These have all been implemented in INIT.LSP")
(pr "using (loadmodule), (getprocaddr), and (peek) :")
(print (help))

;;;
;;; PRINT SOME STATISTICS
;;;
(format stdout
    "~%Date: ~A/~A/~A~%Time: ~A:~A:~A~%"
    (first (date)) (second (date)) (third (date))
    (first (time)) (second (time)) (third (time)))
(format stdout
    "~%Process-id: ~A~%Session: ~A~%"
    (getpid)
    (getgrp))
(format stdout "~%OS2XLISP is running in the ~A~%"
    (if (foreground?)
        "foreground"
        "background"))
(if (not (foreground?))
    (princ (who-is-foreground)))
(format stdout
    "~%MS-DOS real-mode ~A present~%"
    (if (protect-only?) "is not" "is"))
(format stdout
    "~%Available memory: ~A~%"
    (dos-mem-avail))
