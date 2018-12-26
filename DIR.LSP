; (dir) An OS2XLISP file-listing function.
; Andrew Schulman 4-April-1988

; usage: (dir [match-str|ext-sym] [print-flag])
; defaults: match-str is "*.*", print-flag is t
; examples:
;     os2xlisp            os/2 equivalent
;  -----------------      -------------------
;  (dir)            	  c:>dir *.*
;  (dir "*.lsp") 		  c:>dir *.lsp
;  (dir 'lsp)    		  c:>dir *.lsp
;  (dir "*.lsp" nil) 	  none (returns directory info in a list)

; get handles for OS/2 system calls

(define doscalls (loadmodule "DOSCALLS"))
(define DOSFINDFIRST (getprocaddr doscalls "DOSFINDFIRST"))
(define DOSFINDNEXT (getprocaddr doscalls "DOSFINDNEXT"))
(define DOSFINDCLOSE (getprocaddr doscalls "DOSFINDCLOSE"))

; get handle for C runtime-library call

(define crtlib (loadmodule "CRTLIB"))
(define printf (getprocaddr crtlib "_printf"))

; package printf for convenient use

(defmacro printf (mask &rest args)
    `(c-call printf ,mask ,@args))

; OS/2 file-search structure
;
;struct FileFindBuf {
;        unsigned create_date;
;        unsigned create_time;
;        unsigned access_date;
;        unsigned access_time;
;        unsigned write_date;
;        unsigned write_time;
;        unsigned long file_size;
;        unsigned long falloc_size;
;        unsigned attributes;
;        unsigned char string_len;
;        char file_name[13];
;        };

; equivalent OS2XLISP structure

(define FileFindBuf
    '((word create_date)
	  (word create_time)
	  (word access_date)
	  (word access_time)
      (word write_date)
      (word write_time)
      (long file_size)
      (long falloc_size)
      (word attributes)
      (byte string_len)
      ((char 13) file_name)))

; routine to print selected elements of the returned list
          
(define (print-dir filelist) 
   (dotimes (i (length filelist))
   		(printf "%-20s %8lu\n" 
			(cadr (assoc 'file_name (nth i filelist)))
			(cadr (assoc 'file_size (nth i filelist))))))

; directory routine
        
(define (dir &optional filespec (print-flag t))

    (if (null filespec)					 ; establish the filespec
        (setf filespec "*.*"))
    (if (not (equal 'STRING (type-of filespec)))
        (setf filespec (format nil "*.~A" (symbol-name filespec))))

    (let*
         ((filelist nil)				 ; intialize list
         (hdir (word -1))				 ; default directory handle 
		 (attr (word 6))				 ; find normal, hidden, system files
         (buf (make-struct FileFindBuf)) ; make instance of structure
         (buflen (word (length buf)))	 ; OS/2 needs structure's length
         (find-count (word 1)))		     ; find one file at a time
 
    (if (zerop (call DOSFINDFIRST	; get info for first matching file
        ^filespec                          
        ^hdir                            
        attr 	                          
        ^buf                               
        buflen                            
        ^find-count                     
        0))
	; then put info into list, find/add info for remaining files
        (progn
            (setf filelist (list (unpack-struct FileFindBuf ^buf)))
            (while (zerop (call DOSFINDNEXT hdir ^buf buflen ^find-count))
                (nconc filelist
                    (list (unpack-struct FileFindBuf ^buf))))))

    (call DOSFINDCLOSE hdir)

    (if print-flag (print-dir filelist) filelist))) ; print or return the list

