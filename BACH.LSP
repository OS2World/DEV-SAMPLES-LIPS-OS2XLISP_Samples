; bach.lsp -- for OS2XLISP
; Andrew Schulman 25-April-1988
; adapted from versions for Framework II and dBase III Plus (!)
; note that Lisp lets us have variable names like c# instead of csharp

; NOTE!  this is an awful piece of software.  The notes that make up the
; piece should be separated from the code that plays them.  But it will
; do for now.

; "I told you not to ring the cash register during the bass solo"
;		-- Charles Mingus
; or, "garbage collection should never occur while music is playing"
; the upshot being, do a (gc) before we start the music!
(gc)	

(princ "Toccata and Fugue in D Minor by Johann Sebastian Bach...")

(define dosbeep (getprocaddr doscalls "DOSBEEP"))
(define dossleep (getprocaddr doscalls "DOSSLEEP"))

;;; frequencies from Peter Norton, Programmer's Guide to IBM PC, p.147
(define a 440)
(define bflat 467)
(define b 494)
(define c 523)
(define c# 559)
(define d 587)
(define e 659)
(define f 705)
(define f# 746)
(define g 784)

(define d#3 (/ d 4))						

(define (beep f d)
	(call dosbeep (word f) (word d)))

(define (flutter item1 item2 count)
	(dotimes
		(i count)
		(beep item1 40)
		(beep item2 40)))
			
(define (pause p)
	(call dossleep p))

(beep (* a 2) 100)
(beep g 100)
(beep (* a 2) 1200)
(pause 300)		
(beep g 80)
(beep f 80)
(beep e 80)
(beep d 80)
(beep c# 1000)
(beep d 1500)
(pause 900)
(beep a 100)
(beep (/ g 2) 100)
(beep a 1200)
(pause 300)
(beep (/ e 2) 400)
(pause 20)
(beep (/ f 2) 400)
(pause 20)
(beep (/ c# 2) 400)
(pause 20)
(beep (/ d 2) 1500)
(pause 20)
(pause 900)
(beep (/ a 2) 100)
(beep (/ g 4) 100)
(beep (/ a 2) 1200)
(pause 300)
(beep (/ g 4) 80)
(beep (/ f 4) 80)
(beep (/ e 4) 80)
(beep d#3 80)
(beep (/ c# 4) 1000)
(beep d#3 1500)
(flutter d#3 (/ d 8) 8)
(flutter d#3 (/ d 8) 8)
(flutter d#3 (/ c# 4) 2)
(flutter d#3 (/ e 4) 2)
(flutter d#3 (/ g 4) 2)
(flutter d#3 (/ bflat 2) 2)
(flutter d#3 (/ c# 2) 2)
(flutter d#3 (/ e 2) 2)
(flutter d#3 (/ g 2) 2)
(flutter d#3 bflat 2)
(flutter d#3 c# 2)
(flutter d#3 e 2)
(flutter d#3 g 2)
(flutter d#3 (* bflat 2) 3)
(flutter d#3 (* c# 2) 3)
(flutter g (* d 2) 6)
(flutter e (* d 2) 7)
(flutter f# (* d 2) 12)
(beep (* a 2) 40)
(beep f# 40)
(beep (* a 2) 40)
(beep f# 40)
(beep d 40)
(beep f# 40)
(beep d 40)
(beep a 40)
(beep d 40)
(beep a 40)
(beep (/ f# 2) 40)
(beep a 40)
(beep (/ f# 2) 40)
(beep (/ d 2) 40)
(beep (/ f# 2) 40)
(beep (/ d 2) 40)
(beep (/ a 2) 40)
(beep (/ d 2) 40)
(beep (/ a 2) 40)
(beep (/ f# 4) 40)
(beep (/ a 2) 40)
(beep (/ f# 4) 40)
(beep d#3 40)
(beep (/ f# 4) 40)
(beep d#3 40)
(beep (/ a 2) 40)
(beep d#3 1750)

(princ "\n(the abridged version)\n")
(princ "played by E. Power Biggs on your Intel pedal harpsichord\n")
