; enumproc.lsp -- Lisp "callback" functions, etc.
; Andrew Schulman 2-June-1988
; revised 13-June-1988

;======================================================================

; general-purpose enumeration function
; only confusing part:  (if (listp elem) elem (list elem))
; this takes care of two cases:  if elem is NIL, don't put into list;
; and if elem is already list, don't put into list.  works because
; (listp nil) is T.

(define (enum func &rest args)
    (do*
        ((elem (apply func args))                           ; init
         (lst (if (listp elem) elem (list elem))))
        ((not elem) lst)                                    ; test, retval
        (setf elem (apply func nil))                        ; body
        (nconc lst (if (listp elem) elem (list elem)))))
            
;======================================================================

; helper routines

(defmacro dos-call (func &rest args)
    `(call (getprocaddr doscalls ,func) ,@args t))

(define (get-full-path-name dll)
    (if (eq dll "DOSCALLS")
        dll
        (let
            ((handle (word 0))
             (buf (make-string 32 128)))
            (if (dos-call "DOSGETMODHANDLE" dll ^handle)
                (if (dos-call "DOSGETMODNAME" handle (word 128) buf)
                    buf)))))
                        
;======================================================================
                        
; returns list of all functions exported from DLL            
(define (procs dll)
    (enum #'enum-procs (get-full-path-name dll)))
        
;======================================================================

; note that nconc-ing up list pretty unnecessary for (install)
; just included here to show sample use of (procs)
; see real (install) later

(define (old-install dll)
    (let
        ((module (loadmodule dll)))
        (mapcar
            (lambda (p)
                ;;; (print p) ;;; for debugging
                (set
                    (read (make-string-input-stream 
                        (if
                            (char= #\_ (char p 0))
                            (subseq p 1)
                            p))) 
                    (getprocaddr module p)))
             (cdr (procs (get-full-path-name dll))))))
             
;======================================================================

(define strtok (getprocaddr crtlib "_strtok"))

(define (parse s delim)
    (enum
        (lambda (&rest args)
            (if args (define delims (cadr args)))
            (c-call strtok (if args (car args) 0) delims 'str))
        s delim))
            
;======================================================================

(define (install dll &optional print-flag &aux name addr)
    (do*
        ((module (loadmodule dll))                          ; init
         (fullname (get-full-path-name dll))
         (p (enum-procs fullname))
         (ok (progn
                 (if p
                     (format stdout "Installing ~A\n" fullname)
                     (format stdout "Can't install ~A\n" dll))
                 p)))
        ((not p) (if ok t))                                 ; test, retval
        (setf name
            (read (make-string-input-stream
                (if
                    (char= #\_ (char p 0))
                    (subseq p 1)            ; maybe strip leading underscore
                    p))))
        (setf addr (getprocaddr module p))
        (if addr
            (progn
                (if print-flag (format stdout "~A\t" name))
                (set name addr)))
        (setf p (enum-procs))))

; (install "DOSCALLS")
; (install "CRTLIB")


