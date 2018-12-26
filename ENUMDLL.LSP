; enumdll.lsp

(define buf (make-string 32 128))
(define dosgetmodname (getprocaddr doscalls "DOSGETMODNAME"))

(dotimes
	(i #xFFFF)
	(if
		(call dosgetmodname ~i ~128 buf t)
	; then
		(format stdout "~A\t~A\n" i buf)))

;;; sample output:
;;; > (load 'enumdll)
;;; ; loading "ENUMDLL.lsp"
;;; (450 "A:\\HARDERR.EXE")
;;; (620 "D:\\OS2\\SYS\\SHELL.EXE")
;;; (740 "D:\\OS2\\DLL\\BKSCALLS.DLL")
;;; (940 "D:\\OS2\\DLL\\ANSICALL.DLL")
;;; T

; takes a long time, but not much longer than in C!
; perfect to run in the background
