; struct.lsp -- structures for OS2XLISP 
; Andrew Schulman 11-June-1988
; rewrote (sum) and (make-list) without recursion 15-June-1988
; using &aux instead of (let) 17-June-1988
; added support for arrays and imbedded strings 20-July-1988
; added option to (unpack-struct) to return raw data rather than assoc list
; moved comments to struct.doc

;======================================================================
; helper routines

; this interprets 0 as 4 (code for string pointers)
(define (sum lst &aux (sum 0))
    (dolist (elem lst sum)
        (setf sum (+ sum (if (zerop elem) 4 elem)))))

(define (make-list length init &aux (lst nil))
    (if (zerop length) 
        ()
        (dotimes
            (i length lst)
            (setf lst (cons init lst)))))
            
; write non-recursive version later             
; can flatten assoc-lists into property-lists
(define (flatten lst)
    (cond
        ((null lst)
            nil)
        ((atom lst)
            (list lst))
        (t
            (append
                (flatten (car lst))
                (flatten (cdr lst))))))
                    
(define (conv-array s)
    (* (conv (car s) nil) (cadr s)))
    
(define (conv s do-conv)
    (if (listp s)
        (if do-conv
            (conv-array s)
            s)
    ; else
        (case s
            ((0 1 2 4 8)            s)
            ((str string)           0)
            ((byte char)            1)
            ((word int short)       2)
            ((long fixnum ptr)      4)
            ((float flonum double)  8)
            (t (error "Bad structure element type")))))

(define (convert-template template keep)
    (mapcar
        (lambda (size &aux (sz (listp size)))
            (cond
                ((and keep sz)
                    (list (conv (car size) nil) (cadr size)))
                (keep
                    (conv size nil))
                (sz
                    (conv (car size) t))))
            template))

(define (peek-array a s)
    (if (eq 'CHAR (car s))
        ; string (array of CHAR)
        (peek a 0)
        ; other array
        (unpack-struct
            (make-list (cadr s) (conv (car s) nil))
            a)))

;======================================================================
; make packed OS/2-compatible data structure from description in Lisp list
(define (make-struct template &optional data)
    (let*
        ((template (convert-template template nil))
         (str (make-string 32 (sum template)))
         (len (length template))
         (offset 0))
        (cond
            ((not data)
                (setf data (make-list len 0)))
            ((atom data)
                (setf data (make-list len data)))
            (t
                (let
                    ((diff (- len (length data))))
                    (cond
                        ((plusp diff)
                            (nconc data (make-list diff 0)))
                        ((minusp diff)
                            (error "make-struct: template/data mismatch"))))))
        (mapcar
            (lambda (size info)
                ;;; (format stdout "~A ~A\n" size info) ;;; debugging
                (if (zerop size)
                    (setf size 4)
                    (if (member size '(1 2 4 8))
                        (poke (+ ^str offset) info size)
                        ; __temporary__ initialization of arrays
                        (dotimes
                            (i size)
                            (poke (+ ^str i) info 1))))
                (setf offset (+ offset size)))
            template
            data)
        str))

;======================================================================
; turn OS/2 data structure and description into a Lisp assoc list
(define (unpack-struct template str)
    (let
        ((template (convert-template template t))
         (addr (if (eq 'STRING (type-of str)) ^str str))
         (offset 0)
         (s 0)
         (info 0))
        (mapcar
            (lambda (size &aux (a (+ addr offset)))
                (setf s
                    (if (listp size) (car size) size))
                (prog2
                    (setf info
                        (cond
                            ((listp s)                      
                                (peek-array a s))           ; array
                            ((and (zerop s) (not (zerop (peek a 4))))
                                (peek (peek a 4) 0))        ; string ptr
                            (t
                                (peek a s))))               ; normal
;                    (if (and name-flag (listp size))
                         (list (cadr size) info)
;                        info)
                    (setf offset (+ offset
                        (cond
                            ((listp s) (conv-array s))      ; array
                            ((zerop s) 4)                   ; string ptr
                            (t s))))))                      ; normal
            template)))

;======================================================================
; extract data from the structure
(define (get-elem struct instance elem)
    (cadr (assoc elem (unpack-struct struct instance))))
