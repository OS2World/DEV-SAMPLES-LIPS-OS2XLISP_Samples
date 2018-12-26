; initialization file for XLISP 2.0
; for OS2XLISP 1.10
; revised Andrew Schulman 12-June-1988

;======================================================================
(expand 10)

;======================================================================
; define some macros
(defmacro defvar (sym &optional val)
  `(if (boundp ',sym) ,sym (setq ,sym ,val)))
(defmacro defparameter (sym val)
  `(setq ,sym ,val))
(defmacro defconstant (sym val)
  `(setq ,sym ,val))

; (makunbound sym) - make a symbol value be unbound
(defun makunbound (sym) (setf (symbol-value sym) '*unbound*) sym)

; (fmakunbound sym) - make a symbol function be unbound
(defun fmakunbound (sym) (setf (symbol-function sym) '*unbound*) sym)

; (mapcan fun list [ list ]...)
(defmacro mapcan (&rest args) `(apply #'nconc (mapcar ,@args)))

; (mapcon fun list [ list ]...)
(defmacro mapcon (&rest args) `(apply #'nconc (maplist ,@args)))

; (set-macro-character ch fun [ tflag ])
(defun set-macro-character (ch fun &optional tflag)
    (setf (aref *readtable* (char-int ch))
          (cons (if tflag :tmacro :nmacro) fun))
    t)

; (get-macro-character ch)
(defun get-macro-character (ch)
  (if (consp (aref *readtable* (char-int ch)))
    (cdr (aref *readtable* (char-int ch)))
    nil))

; (savefun fun) - save a function definition to a file
(defmacro savefun (fun)
  `(let* ((fname (strcat (symbol-name ',fun) ".lsp"))
          (fval (get-lambda-expression (symbol-function ',fun)))
          (fp (open fname :direction :output)))
     (cond (fp (print (cons (if (eq (car fval) 'lambda)
                                'defun
                                'defmacro)
                            (cons ',fun (cdr fval))) fp)
               (close fp)
               fname)
           (t nil))))

; (debug) - enable debug breaks
(defun debug ()
       (setq *breakenable* t))

; (nodebug) - disable debug breaks
(defun nodebug ()
       (setq *breakenable* nil))

; initialize to enable breaks but no trace back
(setq *breakenable* t)
(setq *tracenable* nil)


;======================================================================
;;; DEFINE -- from the book "T Programming Language", with changes
(defmacro define (name &rest body)
    (cond
        ((atom name)
            `(setq ,name ,@body))
        ((null (cdr (last name)))
            `(defun ,(car name) ,(cdr name) ,@body))))

(defmacro while (test &rest body)
    `(do () ((not ,test)) ,@body))

(defmacro repeat (n &rest expr)
    `(dotimes (i ,n) ,@expr))

(defmacro incr (x) `(setf ,x (1+ ,x)))

(defmacro decr (x) `(setf ,x (1- ,x)))

(defconstant stdout *standard-output*)

(defconstant stdin *standard-input*)


;======================================================================
;;; pointer manipulation macros

(defmacro mk-fp (seg off)
    `(makelong ,seg ,off))
        
(defmacro fp-seg (fp) `(lo-word ,fp))

(defmacro fp-off (fp) `(hi-word ,fp))


;======================================================================
;;; read macros
;;; Dave Betz wrote these -- thanks, Dave!

(set-macro-character #\^
    #'(lambda (stream ch)
        (list `(addr ,(read stream t)))))

(set-macro-character #\~
    #'(lambda (stream ch)
        (list `(word ,(read stream t)))))

;======================================================================
(define doscalls (loadmodule "DOSCALLS"))
(define viocalls (loadmodule "VIOCALLS"))
(define kbdcalls (loadmodule "KBDCALLS"))
(define moucalls (loadmodule "MOUCALLS"))
(define crtlib (loadmodule "CRTLIB"))       ; C 5.1 run-time library DLL

;======================================================================
(define gdt 0)
(define ldt 0)
(call (getprocaddr doscalls "DOSGETINFOSEG") ^gdt ^ldt)
; gdt and ldt have now been "poked" by OS/2

(define (date)
    `(,(peek (mk-fp gdt 17) 'byte)           ; month
    ,(peek (mk-fp gdt 16) 'byte)             ; day
    ,(peek (mk-fp gdt 18) 'int)))            ; year
            
(define (time)
    `(,(peek (mk-fp gdt 8) 'byte)            ; hour
    ,(peek (mk-fp gdt 9) 'byte)              ; minutes
    ,(peek (mk-fp gdt 10) 'byte)))           ; seconds
            
(define (elapsed-time)
    (peek (mk-fp gdt 4) 'long))              ; milliseconds since IPL
        
(define (vers)
    `(,(peek (mk-fp gdt 21) 'byte)           ; major version number
    ,(peek (mk-fp gdt 22) 'byte)             ; minor version number
    ,(peek (mk-fp gdt 23) 'byte)))           ; revision letter
            
(define (foreground-session)
    (peek (mk-fp gdt 24) 'byte))

(define (protect-only?)
    (= 1 (peek (mk-fp gdt 27) 'byte)))
        
(define (foreground-pid)
    (peek (mk-fp gdt 28) 'int))
        
(define (boot-drive)
    (peek (mk-fp gdt 36) 'int))
        
(define (getpid)                ; process id
    (peek (mk-fp ldt 0) 'int))
        
(define (getppid)               ; process id of parent
    (peek (mk-fp ldt 2) 'int))
        
(define (getgrp)                ; screen group/session
    (peek (mk-fp ldt 8) 'int))

(define (priority)
    (peek (mk-fp ldt 4) 'int))
        
(define (thread-id)
    (peek (mk-fp ldt 6) 'int))

(define (subsession)
    (peek (mk-fp ldt 10) 'int))
        
(define (foreground?)
    (not (zerop (peek (mk-fp ldt 12) 'int))))


;======================================================================
(define (dos-mem-avail &aux (mem 0))
    (call
        (getprocaddr doscalls "DOSMEMAVAIL")
        ^mem)
    mem)

;;; SEGMENT INFORMATION PREDICATES          
; is segment present in memory?
(define (present? x)
    (=
        128
        (logand
            (lar x)
            128)))
                
; is segment code?
(define (code? x)
    (=
        8
        (logand
            (lar x)
            8)))

;;; MEMORY ALLOCATION

(define dosallocseg (getprocaddr doscalls "DOSALLOCSEG"))
(define dosfreeseg (getprocaddr doscalls "DOSFREESEG"))

; convoluted expression sometimes necessary to get unique node
(define (new-node x)
    (1+ (1- x)))

(define (dos-alloc-seg size &aux (seg 0))
    (if (zerop (call dosallocseg (word size) (addr seg) (word 0)))
        (new-node seg)))
        
(define (dos-free-seg seg)
     (zerop (call dosfreeseg (word seg))))


;======================================================================
(define (cls) (princ "\033[2J"))

(define (set-cursor row col)
    (format stdout "\033[~A;~AH" row col))


;======================================================================
;;; structures
(load 'struct)

;======================================================================
; miscellaneous stuff

;;; convert from XLISP file stream to OS/2 DOS file handle
(define (fileno f)
    (peek (+ 11 ^f) 1))

;;; replacement for old (peek)
;;; no longer used, but kept in as illustrating of writing front-ends
;;; for built-ins.  If you don't like a function, write a front-end for it!
;    (if (not (boundp 'old-peek))
;        (define old-peek #'peek))               ; same away old function ptr
;    (define (peek place &optional (arg 1))      ; define new func
;         (funcall old-peek                      ; call the old func,
;            (if (listp place)                   ; but rewrite the arguments
;                (mk-fp (car place) (cadr place))    
;                place)
;            (case arg
;                ((str string)   0)
;                ((byte)         1)
;                ((word int)     2)
;                ((long fixnum)  4)
;                ((float double) 8)
;                (t              arg))))

; for compatibility with old versions of OS2XLISP               
(define (register library function)
    (getprocaddr
        (loadmodule library)
        function))

;;; uncomment these next two lines if you want a different prompt
; (define *promptcount* 0)
; (define *prompt* '(format stdout "[~A] " (incr *promptcount*)))


;======================================================================
; (format stdout 
;    "~A/~A/~A   ~A:~A:~A~%"
;    (first (date)) (second (date)) (third (date))
;    (first (time)) (second (time)) (third (time)))

