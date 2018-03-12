(declare (usual-integrations))
(load-option 'ffi)
(C-include "netcdf")

;; below should probably go into a file somewhere
(define (zero)
  (identity-procedure 0.))

(define nan
  (flo:with-exceptions-untrapped (flo:exception:invalid-operation)
                                 (lambda ()
                                   (flo:/ (zero) (zero)))))

(define inf+
  (flo:with-exceptions-untrapped (flo:exception:divide-by-zero)
    (lambda ()
      (flo:/ +1. (zero)))))

(define inf-
  (flo:with-exceptions-untrapped (flo:exception:divide-by-zero)
    (lambda ()
      (flo:/ -1. (zero)))))

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
          filename
          (load-ncid filename))))

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

(define (get-ncid metadata)
  (fifth metadata))


;;; C-lib functions
;; below lifted from x11-base.scm
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

(define (load-ncid filename)
  (let* ((alien-ncid (malloc (* 2 (c-sizeof "int")) 'int))
         (out (C-call "nc_open" (->cstring filename) 0 alien-ncid)))
    (cond ((= 0 out) (newline) (display "loading ncid sucessful"))
          ((= -61 out) (error "not enough memory" filename))
          ((= -101 out) (error "Error at HDF5 layer" filename))
          ((= -106 out) (error "Problem with dimension metadata" filename))
          ((alien-null? alien-ncid)
           (error "could't open file-unspecified reason" filename))
          (else (error "unspecified response")))
    (C-> alien-ncid "int")))

(define (load-varid metadata var-name)
  (let* ((alien-varid (malloc (* 2 (c-sizeof "int")) 'int))
         (ncid (get-ncid metadata))
         (out (C-call "nc_inq_varid" ncid (->cstring var-name) alien-varid)))
    (cond ((= 0 out) (newline) (display "loading varid sucessful"))
          ((= -33 out) (error "Not a netcdf id." ncid))
          ((alien-null? alien-varid)
           (error "could't open file-unspecified reason" ncid))
          (else (error "unspecified response")))
    (C-> alien-varid "int")))

(define (close-ncid metadata)
  ;; this is dangerous, introduces state :(
  (let* ((ncid (get-ncid metadata))
         (out (C-call "nc_close" ncid)))
    (cond ((= 0 out) (newline) (display "closing sucessful"))
          ((= -33 out) (error "Not a netcdf id." ncid))
          ((= -116 out) (error "Bad group ID." ncid))
          (else (error "unspecified response")))
    #f))

(define (load-var metadata varid nelements)
  (let* ((ncid (get-ncid metadata))
         (alien-var (malloc (* 2 nelements (c-sizeof "short")) 'short))
         (out (c-call "nc_get_var_short" ncid varid alien-var)))
    (cond ((= 0 out) (newline) (display "loading var sucessful"))
          ((= -49 out) (error "Variable not found" varid))
          ((= -60 out) (error "Math result not representable" varid))
          ((= -39 out) (error "Operation not allowed in define mode" varid))
          ((= -33 out) (error "Not a netcdf id" varid))
          ((alien-null? alien-ncid)
           (error "could't load var- unspecified reason" varid))
          (else (error "unspecified response")))
    alien-var))

(define (alien-array->vector alien nelements process)
  (let ((vec (make-vector nelements -99999.0)))
    ;; below using cons maximum recursion depth is exceeded
    (let loop ((n 0))
      (if (< n nelements)
          (let ((short (C-> alien "short")))
            (vector-set! vec n (process short))
            (C-array-loc! alien "short" 1)
            (loop (1+ n)))))
    vec))

;; below you need large stack allocation to avoid
;; max recursion depth
(define (alien-array->list alien nelements peek advance)
  (let loop ((n 0))
    (if (< n nelements)
        (let ((value (peek alien)))
          (advance alien)
          (cons value (loop (1+ n))))
        '())))

(define (load-var-meta metadata varid)
  (let* ((ncid (get-ncid metadata))
         (alien-name (malloc (* 80 (c-sizeof "char")) '(* char)))
         (alien-xtype (malloc (* 2 (c-sizeof "int")) 'nc_type))
         (alien-ndims (malloc (* 2 (c-sizeof "int")) 'int))
         (alien-dimids (malloc (* 10 2 (c-sizeof "int")) 'int))
         (alien-natts (malloc (* 2 (c-sizeof "int")) 'int))
         (out (c-call "nc_inq_var" ncid varid alien-name
                      alien-xtype alien-ndims alien-dimids
                      alien-natts)))
    (cond ((= 0 out) (newline) (display "loading var sucessful"))
          ((= -49 out) (error "Variable not found" varid))
          ((= -33 out) (error "Not a netcdf id" varid))
          ((alien-null? alien-ndims)
           (error "could't load var- unspecified reason" varid))
          (else (error "unspecified response")))
    (make-var-structure metadata alien-name alien-xtype alien-ndims
                        alien-dimids alien-natts)))

(define (make-var-structure metadata alien-name alien-xtype alien-ndims
                            alien-dimids alien-natts)
  (let ((ndims (c-> alien-ndims "int") ))
    `((filename . ,(get-filepath metadata))
      (ncid . ,(get-ncid metadata))
      (name . ,(alien->string alien-name))
      (xtype . ,(alien->type alien-xtype))
      (ndims . ,ndims)
      (dimids . ,(alien-array->list alien-dimids ndims
                                    (lambda (x) (c-> x "int"))
                                    (lambda (x) (C-array-loc! x "int" 1))))
      (natts . ,(c-> alien-natts "int")))))

(define (alien->string alien)
  (let ((new (c-peek-cstring alien)))
    (if (alien-null? alien)
        (error "No string in pointer."))
    (if (string? new)
        new
        (utf8->string new))))

(define (alien->type alien)
  (let ((type (c-> alien "int"))
        (conv-key '((0 . "not a type") (1 . "byte") (2 . "char")
                    (3 . "short") (4 . "int") (5 . "float")
                    (6 . "double") (7 . "ubyte") (8 . "ushort")
                    (9 . "uint") (10 . "uint64") (11 . "uint64")
                    (12 . "string"))))
    (cdr (assoc type conv-key))))

(define (irwin-processor element)
  (if (eqv? element -31999)
      nan
      (let ((value (+ 200.0 (* element 0.01))))
        (if (or (< value 140.0) (> value 375.0))
            inf-;;(error "value out of expected range")
            value))))

;; (define vec (alien-array->vector alien-var nelements irwin-processor))
;; (define vec-list (vector->list vec))
;;(close-ncid ncid)

