; segs.lsp
; functions to examine code segments of XLISP
; Andrew Schulman 27-Feb-1988

; address of C function stored in XLISP node
(defun func-addr (x)
	(peek (address-of x 2) 4))
		
; segment/module where C function lived
(defun func-seg (x)
	(fp-seg (func-addr x)))
		
; requested protection level
(defun rpl (x)
	(logand 3 x))
		
; has segment been accessed?
(defun accessedp (x)
	(=
		1
		(logand
			(lar x)
			1)))

; is segment present in memory?
(defun presentp (x)
	(=
		128
		(logand
			(lar x)
			128)))
				
; is segment code?
(defun codep (x)
	(=
		8
		(logand
			(lar x)
			8)))

; is segment readable code?				
(defun readp (x)
	(and 
		(codep x)
		(=
			2
			(logand
				(lar x)
				2))))
					
; is segment writable code?				
(defun writep (x)
	(and 
		(not (codep x))
		(=
			2
			(logand
				(lar x)
				2))))
