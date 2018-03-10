(declare (usual-integrations))
(load-option 'ffi)
(C-include "netcdf")

;;;; This uses ncdump to take a quick look at file contents
(define (make-metadata filename)
  (let*
      ((string
         (call-with-output-string (lambda (port)
                                    (run-shell-command
                                     (string-append "ncdump -h " filename)
                                     'output port))))
       (string-list
        ((string-splitter 'delimiter #\newline 'copy? #t) string))
       (joiner
        (string-joiner 'infix "\n")))
    (define (split-list newline-list startstr endstr)
      ;; takes a list of ncdump's output split on new line
      ;; returns a list spanning startstr to endstr
      (define (make-section section-list)
        (if (string=? (car section-list) endstr)
            '()
            (cons (car section-list) (make-section (cdr section-list)))))
      (if (string=? (car newline-list) startstr)
          (make-section newline-list)
          (split-list (cdr newline-list) startstr endstr)))
    (define (var-filter? line)
      (if (or (substring? "long_name" line)
              (substring? "float" line)
              (substring? "int" line)
              (substring? "char" line)
              (substring? "short" line)
              (substring? "byte" line)
              (string=? "variables:" line))
          #t
          #f))
    (list (apply joiner (split-list string-list "dimensions:" "variables:"))
          (apply joiner (map (lambda (string)
                               ;;(string-pad-right string 75)
                               string)
                             (filter var-filter?
                                     (split-list string-list "variables:"
                                                 "// global attributes:"))))
          (apply joiner (split-list string-list "// global attributes:" "}"))
          filename)))

(define (get-dimensions metadata)
  (let ((dims (car metadata)))
    (display dims)
    dims))
(define (get-variables metadata)
  (let ((vars (cadr metadata)))
    (display vars)
    vars))
(define (get-attributes metadata)
  (let ((attrs (caddr metadata)))
    (display attrs)
    attrs))
(define (get-filepath metadata)
  (cadddr metadata))


;; testing
(define meta (make-metadata
              (string-append "/home/adam/scratch/data/"
                             "isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc")))
(display (get-filepath meta))
(define dims (get-dimensions meta))
(define vars (get-variables meta))
(define attrs (get-attributes meta))


;;; now we try loading with c lib
(load-option 'ffi)
(C-include "netcdf")

(define string (let* ((alien (make-alien
                              '(* char))))
                 (C-call "nc_inq_libvers" alien)
                 (let ((new (c-peek-cstring alien)))
                   (if (alien-null? alien)
                       (error "Could not open-file."))
                   (if (string? new)
                       new
                       (utf8->string new)))))
(display string)

;; lefted crom x11-base.scm
(define (->cstring string)
  (cond ((and (integer? string) (zero? string))
	 0)
	((bytevector? string)
	 (if (let ((end (bytevector-length string)))
	       (let loop ((i 0))
		 (if (fix:< i end)
		     (or (fix:zero? (bytevector-u8-ref string i))
			 (loop (fix:1+ i)))
		     #f)))
	     string
	     (error "C string not null terminated:" string)))
	((string? string)
	 ;; String->iso8859-1 would be incorrect; it does not null terminate.
	 (let* ((end (string-length string))
		(result (make-bytevector (fix:1+ end))))
	   (do ((i 0 (fix:1+ i)))
	       ((not (fix:< i end))
		(bytevector-u8-set! result i #x00))
	     (bytevector-u8-set! result i (char->integer
					   (string-ref string i))))
	   result))
	(else
	 (error:wrong-type-argument string "a string or 0" '->cstring))))

;;"/home/adam/scratch/data/isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc"
(C-include "netcdf")
(let* ((alien-out (make-alien 'int))
       (alien-mode (malloc (c-sizeof "int") 'int))
       (alien-ncid (malloc (* 2 (c-sizeof "int")) 'int)))
  (display "test")
  (define out (C-call "nc_open"
                      (->cstring (string-append
                                  "/home/adam/scratch/data/"
                                  "isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc"))
                      0
                      alien-ncid))
  (display "test")
  (newline) (display out)
  (if (alien-null? alien-ncid)
      (display "eroor could't open")
      (display "did it"))
  (newline) (display (C-> alien-ncid "int"))
  (define out2 (C-call "nc_close" (C-> alien-ncid "int")))
  (newline) (display out2))

;; maybe try that alloc bytevector thing with char
