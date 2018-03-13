(declare (usual-integrations))
(load-option 'ffi)
(C-include "netcdf")


;; below should probably go into a file somewhere
;; useful definitions
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

(define (pair key value)
  `(,key . ,value))


;; file-level metadata (using ncdump rather than c ffi)
(define (make-meta filename)
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
    `((dimensions
       . ,(apply joiner (split-list string-list "dimensions:" "variables:")))
      (variables
       . ,(apply joiner (map (lambda (string)
                               ;;(string-pad-right string 75)
                               string)
                             (filter var-filter?
                                     (split-list string-list "variables:"
                                                 "// global attributes:")))))
      (attributes
       . ,(apply joiner (split-list string-list "// global attributes:" "}")))
      (filename . ,filename)
      (ncid . ,(load-ncid filename)))))

(define (get-meta-element key metadata)
  (cdr (assoc key metadata)))


;;;; General C-lib functions
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

;; file-level c fucntions
(define (load-ncid filename)
  (let* ((alien-ncid (malloc (c-sizeof "int") 'int))
         (out (C-call "nc_open" (->cstring filename) 0 alien-ncid)))
    (cond ((= 0 out) (newline) (display "loading ncid sucessful"))
          ((= -61 out) (error "not enough memory" filename))
          ((= -101 out) (error "Error at HDF5 layer" filename))
          ((= -106 out) (error "Problem with dimension metadata" filename))
          ((alien-null? alien-ncid)
           (error "could't open file-unspecified reason" filename))
          (else (error "unspecified response")))
    (C-> alien-ncid "int")))

(define (close-ncid metadata)
  ;; this is dangerous, introduces state, :(, run when done w/ file
  (let* ((ncid (get-meta-element 'ncid metadata))
         (out (C-call "nc_close" ncid)))
    (cond ((= 0 out) (newline) (display "closing sucessful"))
          ((= -33 out) (error "Not a netcdf id." ncid))
          ((= -116 out) (error "Bad group ID." ncid))
          (else (error "unspecified response")))
    #f))

;; below you need large stack allocation to avoid
;; max recursion depth
(define (alien->list alien nelements type)
  (let ((peek (get-peek type))
        (advance (get-advance type)))
    (let loop ((n 0))
      (if (< n nelements)
          (let ((value (peek alien)))
            (advance alien)
            (cons value (loop (1+ n))))
          '()))))

(define (alien->string alien)
  (let ((new (c-peek-cstring alien)))
    (if (alien-null? alien)
        (error "No string in pointer."))
    (if (string? new)
        new
        (utf8->string new))))

(define (alien->type alien)
  (let ((type (c-> alien "int")))
    (cdr (assoc type +type-conv-key+))))


;;;; variable-level c functions
(define (load-varid metadata var-name)
  (let* ((alien-varid (malloc (c-sizeof "int") 'int))
         (ncid (get-meta-element 'ncid metadata))
         (out (C-call "nc_inq_varid" ncid (->cstring var-name) alien-varid)))
    (cond ((= 0 out) (newline) (display "loading varid sucessful"))
          ((= -33 out) (error "Not a netcdf id." ncid))
          ((alien-null? alien-varid)
           (error "could't open file-unspecified reason" ncid))
          (else (error "unspecified response")))
    (C-> alien-varid "int")))

;; (define (load-var metadata varid nelements)
;;   )

(define (load-var-meta metadata varname)
  (let* ((ncid (get-meta-element 'ncid metadata))
         (varid (load-varid metadata varname))
         (alien-name (malloc (* 80 (c-sizeof "char")) '(* char)))
         (alien-xtype (malloc (c-sizeof "int") 'nc_type))
         (alien-ndims (malloc (c-sizeof "int") 'int))
         ;; below max 10 dims, but should really call to get ndims first
         (alien-dimids (malloc (* 10 (c-sizeof "int")) 'int))
         (alien-natts (malloc (c-sizeof "int") 'int))
         (out (c-call "nc_inq_var" ncid varid alien-name
                      alien-xtype alien-ndims alien-dimids
                      alien-natts)))
    (cond ((= 0 out) (newline) (display "loading var sucessful"))
          ((= -49 out) (error "Variable not found" varid))
          ((= -33 out) (error "Not a netcdf id" varid))
          ((alien-null? alien-ndims)
           (error "could't load var- unspecified reason" varid))
          (else (error "unspecified response")))
    (let* ((var-meta
           (make-var-structure metadata varid alien-name alien-xtype alien-ndims
                               alien-dimids alien-natts))
           (var-meta (load-dims var-meta))
           (att-names (load-att-names var-meta))
           (var-meta (add-var-structure 'att
                                        (map (lambda (name)
                                               (load-att var-meta name))
                                             att-names)
                                        var-meta)))
      var-meta)))

(define (make-var-structure metadata varid alien-name alien-xtype alien-ndims
                            alien-dimids alien-natts)
  (let ((ndims (c-> alien-ndims "int") ))
    `((filename . ,(get-meta-element 'filename metadata))
      (ncid . ,(get-meta-element 'ncid metadata))
      (name . ,(alien->string alien-name))
      (varid . ,varid)
      (xtype . ,(alien->type alien-xtype))
      (ndims . ,ndims)
      (dimids . ,(alien->list alien-dimids ndims "int"))
      (natts . ,(c-> alien-natts "int")))))

;; (define (build-loader var-meta)
;;   (let* ((ncid (get-var-element 'ncid var-meta))
;;          (nelements (apply * (alist->list (get-var-element 'dims var-meta))))
;;          (type (get-var-element 'xtype var-meta))
;;          (alien-var (malloc (* nelements (c-sizeof type))
;;                             (string->symbol type)))
;;          (out (c-call (string-append "nc_get_var_" type) ncid varid alien-var)))
;;     (cond ((= 0 out) (newline) (display "loading var sucessful"))
;;           ((= -49 out) (error "Variable not found" varid))
;;           ((= -60 out) (error "Math result not representable" varid))
;;           ((= -39 out) (error "Operation not allowed in define mode" varid))
;;           ((= -33 out) (error "Not a netcdf id" varid))
;;           ((alien-null? alien-ncid)
;;            (error "could't load var- unspecified reason" varid))
;;           (else (error "unspecified response")))
;;     alien-var))

;; (define (irwin-processor element)
;;   (if (eqv? element -31999)
;;       nan
;;       (let ((value (+ 200.0 (* element 0.01))))
;;         (if (or (< value 140.0) (> value 375.0))
;;             nan;;(error "value out of expected range")
;;             value))))

;; (define (build-processor var-meta)
;;   (let ((attrs (get-var-element 'att-names var-meta)))
;;     (if (assoc "_FillValue" attrs)
;;         (define fvalue (load-att var-meta "_FillValue")))
;;     (lambda (value)
;;       )))

(define (add-var-structure key value var-structure)
  (cons (pair key value) var-structure))

(define (key-exists? key structure)
  (assoc key structure))

(define (alist->list structure)
  (map cdr structure))

;;;; dimension functions
(define (load-dims var-meta)
  (if (key-exists? 'dims var-meta)
      (error "Called load-dims but dims already exists" var-meta)
      (add-var-structure
       'dims
       (map (lambda (dimid)
              (load-dim (get-var-element 'ncid var-meta) dimid))
            (get-var-element 'dimids var-meta))
       var-meta)))

(define (load-dim ncid dimid)
  (let* ((alien-name (malloc (* 80 (c-sizeof "char")) '(* char)))
        (alien-len (malloc (c-sizeof "ulong") '(* size_t)))
        (out (c-call "nc_inq_dim" ncid dimid alien-name alien-len)))
    (cond ((= 0 out) (newline) (display "loading var sucessful"))
          ((= -46 out) (error "Bad dim name" dimid))
          ((= -33 out) (error "Not a netcdf id" ncid))
          ((alien-null? alien-len)
           (error "could't load var- unspecified reason" varid))
          (else (error "unspecified response")))
    (pair (alien->string alien-name) (c-> alien-len "ulong"))))

;;;; attribute funtions
(define (load-att-names var-meta)
  (if (key-exists? 'att-names var-meta)
      (error "Called load-att-names but attname already exists" var-meta)
      (map (lambda (attnum)
             (load-attname (get-var-element 'ncid var-meta)
                           (get-var-element 'varid var-meta)
                           attnum))
           (list-tabulate (get-var-element 'natts var-meta) (lambda (x) x)))))

(define (load-att var-meta name)
  (let* ((ncid (get-var-element 'ncid var-meta))
         (varid (get-var-element 'varid var-meta))
         (xtype (malloc (c-sizeof "int") 'nc_type))
         (len (malloc (c-sizeof "ulong") 'size_t))
         (out (c-call "nc_inq_att" ncid varid name xtype len)))
    (if (not (equal? out 0))
        (error "failed to load attribute metadata code:" out))
    (let* ((nelements (c-> len "ulong"))
           (type (alien->type xtype))
           (loader
            (lambda ()
              (let* ((alien-data (malloc (* nelements
                                           (get-sizeof type))
                                        (string->symbol type)))
                     (out (c-call "nc_get_att" ncid
                                 varid name alien-data)))
                (if (not (equal? out 0))
                    (error "failed to load attribute, code: " out)
                    (if (equal? type "char")
                        (utf8->string
                         (list->bytevector
                          (alien->list alien-data nelements type)))
                        (alien->list alien-data nelements type)))))))
      ;; (loader)
      (pair name ;(list name type nelements)
            (loader))
      )))

(define (load-attname ncid varid attnum)
  (let* ((alien-name (malloc (* 80 (c-sizeof "char")) '(* char)))
         (out (c-call "nc_inq_attname" ncid varid attnum alien-name)))
    (cond ((= 0 out) (newline) (display "loading var sucessful"))
          ((= -33 out) (error "Not a netcdf id" ncid))
          ((= -49 out) (error "Bad varid" varid))
          ((= -116 out) (error "Bad group id" varid))
          ;; note there are more error codes, im too lazy to do
          ;; I should have made a univsersal error-check gen func
          ((alien-null? alien-name)
           (error "could't load att-name - unspecified reason" varid))
          (else (error "unspecified response")))
    (alien->string alien-name)))

(define (get-var-element key var-structure)
  (let ((value (assoc key var-structure)))
    (if value
        (cdr value)
        (error "key not in structure" (list key (map car var-structure))))))


;;;; misc. temp. functions



(define +type-conv-key+ '((0 . "not a type") (1 . "byte") (2 . "char")
                        (3 . "short") (4 . "int") (5 . "float")
                        (6 . "double") (7 . "ubyte") (8 . "ushort")
                        (9 . "uint") (10 . "uint64") (11 . "uint64")
                        (12 . "string")))


;; (define +supported-types+ '("char" "uchar" "short" "ushort"
;;                             "int" "uint" "long" "ulong" "float" "double"))

;; doesn't let us pass string values, not sure why
;; (define (gen-peek name)
;;   (pair name (lambda (x)
;;                (c-> x "int"))))
;; (define (gen-advance name)
;;   (pair name (lambda (x)
;;                (c-array-loc! x "int" 1))))

;; (define +peek-list+ (map gen-peek +supported-types+))
;; (define +advance-list+ (map gen-advance +supported-types+))

(define (get-peek type)
  (cdr (assoc type +peek-list+)))

(define (get-advance type)
  (cdr (assoc type +advance-list+)))

(define (get-sizeof type)
  (cdr (assoc type +size-list+)))

(define +peek-list+ `(("char" . ,(lambda (x) (c-> x "char")))
                      ("uchar" . ,(lambda (x) (c-> x "uchar")))
                      ("short" . ,(lambda (x) (c-> x "short")))
                      ("ushort" . ,(lambda (x) (c-> x "ushort")))
                      ("int" . ,(lambda (x) (c-> x "int")))
                      ("uint" . ,(lambda (x) (c-> x "uint")))
                      ("long" . ,(lambda (x) (c-> x "long")))
                      ("ulong" . ,(lambda (x) (c-> x "ulong")))
                      ("float" . ,(lambda (x) (c-> x "float")))
                      ("double" . ,(lambda (x) (c-> x "double")))))

(define +advance-list+ `(("char" . ,(lambda (x) (c-array-loc! x "char" 1)))
                         ("uchar" . ,(lambda (x) (c-array-loc! x "uchar" 1)))
                         ("short" . ,(lambda (x) (c-array-loc! x "short" 1)))
                         ("ushort" . ,(lambda (x) (c-array-loc! x "ushort" 1)))
                         ("int" . ,(lambda (x) (c-array-loc! x "int" 1)))
                         ("uint" . ,(lambda (x) (c-array-loc! x "uint" 1)))
                         ("long" . ,(lambda (x) (c-array-loc! x "long" 1)))
                         ("ulong" . ,(lambda (x) (c-array-loc! x "ulong" 1)))
                         ("float" . ,(lambda (x) (c-array-loc! x "float" 1)))
                         ("double" .
                          ,(lambda (x) (c-array-loc! x "double" 1)))))

(define +size-list+ `(("char" . ,(c-sizeof "char"))
                      ("uchar" . ,(c-sizeof "uchar"))
                      ("short" . ,(c-sizeof "short"))
                      ("ushort" . ,(c-sizeof "ushort"))
                      ("int" . ,(c-sizeof "int"))
                      ("uint" . ,(c-sizeof "uint"))
                      ("long" . ,(c-sizeof "long"))
                      ("ulong" . ,(c-sizeof "ulong"))
                      ("float" . ,(c-sizeof "float"))
                      ("double" . ,(c-sizeof "double"))))


#f


