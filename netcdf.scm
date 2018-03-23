;;; ----------------------------------------------------------------------
;;; Copyright 2018 Adam Massmann.
;;; ----------------------------------------------------------------------
;;; This file is part of scheme-netcdf.
;;;
;;; scheme-netcdf is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; scheme-netcdf is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with scheme-netcdf.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

(declare (usual-integrations))
(load-option 'ffi)
(load-option 'wt-tree)
(load-option 'gdbm)
(C-include "netcdf")

(import-gdbm2)
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

;; (define (pair key value)
;;   `(,key . ,value))
(define (pair key value)
  (if (not (list? value))
      (cons key (list value))
      (cons key value)))


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
        (if (or  (string=? (car section-list) endstr)
                 (string=? (car section-list) "}"))
            '()
            (cons (car section-list) (make-section (cdr section-list)))))
      (if (string=? (car newline-list) startstr)
          (make-section newline-list)
          (if (null? (cdr newline-list))
              (list startstr "has no values")
              (split-list (cdr newline-list) startstr endstr))))
    
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
       . (,(apply joiner (split-list string-list "dimensions:" "variables:"))))
      (variables
       . (,(apply joiner (map (lambda (string)
                                ;;(string-pad-right string 75)
                                string)
                              (filter var-filter?
                                      (split-list string-list "variables:"
                                                  "// global attributes:"))))))
      (attributes
       . (,(apply joiner (split-list string-list "// global attributes:" "}"))))
      (filename . (,filename))
      (ncid . (,(load-ncid filename))))))


(define (get-element key structure)
  (let ((value (assoc key structure)))
    (if value
        (let ((output (cdr value)))
          (if (equal? (length output) 1)
              (car output)
              output))
        (error "key not in structure" (list key (map get-key structure))))))

(define (get-element-list key structure)
  ;; differs from above in that gaurantees to send a list
  ;; e.g. if you need it for map
  (let ((value (assoc key structure)))
    (if value
        (cdr value)
        (error "key not in structure" (list key (map get-key structure))))))

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
          (else (display "WARNING: unspecified response: ") (display out)))
    (C-> alien-ncid "int")))

(define (close-ncid metadata)
  ;; this is dangerous, introduces state, :(, run when done w/ file
  (let* ((ncid (get-element 'ncid metadata))
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
         (ncid (get-element 'ncid metadata))
         (out (C-call "nc_inq_varid" ncid (->cstring var-name) alien-varid)))
    (cond ((= 0 out) (newline) (display "loading varid sucessful"))
          ((= -33 out) (error "Not a netcdf id." ncid))
          ((alien-null? alien-varid)
           (error "could't open file-unspecified reason" ncid))
          (else (error "unspecified response")))
    (C-> alien-varid "int")))

(define (load-var-meta metadata varname)
  (let* ((ncid (get-element 'ncid metadata))
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
    (cond ((= 0 out)) ;(newline) (display "loading var sucessful")
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
           (var-meta (add-element 'atts
                                        (map (lambda (name)
                                               (load-att var-meta name))
                                             att-names)
                                        var-meta)))
      var-meta)))

(define (make-var-structure metadata varid alien-name alien-xtype alien-ndims
                            alien-dimids alien-natts)
  (let ((ndims (c-> alien-ndims "int") ))
    `((filename . (,(get-element 'filename metadata)))
      (ncid . (,(get-element 'ncid metadata)))
      (name . (,(alien->string alien-name)))
      (varid . (,varid))
      (xtype . (,(alien->type alien-xtype)))
      (ndims . (,ndims))
      (dimids . ,(alien->list alien-dimids ndims "int"))
      (natts . (,(c-> alien-natts "int"))))))

(define (make-var-data meta name)
  (let* ((var-meta (load-var-meta meta name))
         (var-data (load-var-data var-meta)))
    (list (pair 'data var-data)
          (pair 'meta var-meta)
          (pair 'type 'raw))))

(define (raw? variable);check
  (if (equal? 'raw (get-element 'type variable)) #t #f))
(define (dim-loaded? variable);check
  (if (equal? 'dim-loaded (get-element 'type variable)) #t #f))
(define (dim-tagged? variable)
  (if (equal? 'dim-tagged (get-element 'type variable)) #t #f))
(define (wt-tree? variable);check
  (if (equal? 'wt-tree (get-element 'type variable)) #t #f))

;; below need to implement
(define (array? variable)
  (if (equal? 'array (get-element 'type variable)) #t #f))
(define (hast-table? variable)
  (if (equal? 'hast-table (get-element 'type variable)) #t #f))
(define (vector? variable)
  (if (equal? 'vector (get-element 'type variable)) #t #f))

(define (remove-type variable)
  (del-assoc 'type variable))

(define (tag-type tag variable)
  (if (assoc 'type variable)
      (cons (pair 'type tag) (remove-type variable))
      (cons (pair 'type tag))))

(define (load-var-data var-meta)
  (let* ((ncid (get-element 'ncid var-meta))
         (varid (get-element 'varid var-meta))
         (nelements (apply * (alist->list (get-element-list 'dims var-meta))))
         (type (get-element 'xtype var-meta))
         (alien-var (malloc (* nelements (get-c-sizeof type))
                            (string->symbol type)))
         (out (c-call "nc_get_var" ncid varid alien-var)))
    (cond ((= 0 out)) ; (newline) (display "loading var sucessful")
          ((= -49 out) (error "Variable not found" varid))
          ((= -60 out) (error "Math result not representable" varid))
          ((= -39 out) (error "Operation not allowed in define mode" varid))
          ((= -33 out) (error "Not a netcdf id" varid))
          ((alien-null? alien-ncid)
           (error "could't load var- unspecified reason" varid))
          (else (error "unspecified response")))
    (map (build-processor var-meta)
         (alien->list alien-var nelements type))))

(define (build-processor var-meta)
  (let* ((attrs (get-element 'atts var-meta))
         (fillv (if (assoc "_FillValue" attrs)
                    (get-element "_FillValue" attrs)
                    (begin (newline) (display "WARNING - no fill value") nan)))
         (offset (if (assoc "add_offset" attrs)
                     (get-element "add_offset" attrs)
                     0))
         (scale (if (assoc "scale_factor" attrs)
                     (get-element "scale_factor" attrs)
                     1))
         (valid_range (if (assoc "valid_range" attrs)
                          (get-element "valid_range" attrs)
                          (list inf- inf+)))
         (min (car valid_range))
         (max (cadr valid_range)))
    (lambda (element)
      (if (eqv? element fillv)
          nan
          (let ((value (+ offset (* element scale))))
            (if (or (< value min) (> value max))
                nan ;;(error "value out of expected range")
                value))))))

(define (add-element key value structure)
  (cons (pair key value) structure))

(define (key-exists? key structure)
  (assoc key structure))

(define (alist->list structure)
  (map (lambda (x) (car (get-value x))) structure))

(define (get-keys structure)
  (map get-key structure))

;;;; dimension functions
(define (load-dims var-meta)
  (if (key-exists? 'dims var-meta)
      (error "Called load-dims but dims already exists" var-meta)
      (add-element
       'dims
       (map (lambda (dimid)
              (load-dim (get-element 'ncid var-meta) dimid))
            (get-element-list 'dimids var-meta))
       var-meta)))

(define (load-dim ncid dimid)
  (let* ((alien-name (malloc (* 80 (c-sizeof "char")) '(* char)))
        (alien-len (malloc (c-sizeof "ulong") '(* size_t)))
        (out (c-call "nc_inq_dim" ncid dimid alien-name alien-len)))
    (cond ((= 0 out)) ;(newline) (display "loading var sucessful")
          ((= -46 out) (error "Bad dim name" dimid))
          ((= -33 out) (error "Not a netcdf id" ncid))
          ((alien-null? alien-len)
           (error "could't load var- unspecified reason" varid))
          (else (error "unspecified response")))
    (pair (alien->string alien-name) (c-> alien-len "ulong"))))

;;;; attribute funtions
(define (load-att-names var-meta)
  (map (lambda (attnum)
         (load-attname (get-element 'ncid var-meta)
                       (get-element 'varid var-meta)
                       attnum))
       (list-tabulate (get-element 'natts var-meta) (lambda (x) x))))

(define (load-att var-meta name)
  (let* ((ncid (get-element 'ncid var-meta))
         (varid (get-element 'varid var-meta))
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
                                           (get-c-sizeof type))
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
    (cond ((= 0 out)) ;(newline) (display "loading var sucessful")
          ((= -33 out) (error "Not a netcdf id" ncid))
          ((= -49 out) (error "Bad varid" varid))
          ((= -116 out) (error "Bad group id" varid))
          ;; note there are more error codes, im too lazy to do
          ;; I should have made a univsersal error-check gen func
          ((alien-null? alien-name)
           (error "could't load att-name - unspecified reason" varid))
          (else (error "unspecified response")))
    (alien->string alien-name)))

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

(define (get-c-sizeof type)
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




(define (and-list x)
  (if (null? x)
      #t
      (if (car x)
          (and-list (cdr x))
          #f)))


;;;; conversion scripts
(define (var-exists? ncid var-name)
  (let* ((alien-varid (malloc (c-sizeof "int") 'int))
         (out (C-call "nc_inq_varid" ncid (->cstring var-name) alien-varid)))
    (cond ((= -33 out) (error "Bad netcdf id on test-var" ncid))
          ((= 0 out) #t)
          (else #f))))

(define (add-dims variable)
  (let* ((meta (get-element 'meta variable))
         (ncid (get-element 'ncid meta))
         (dims (get-element 'dims meta))
         (keys (get-keys dims))
         (vals (alist->list dims))
         (loaded-dims
          (if (and-list (map (lambda (key)
                               (var-exists? ncid key))
                             keys))
              (map (lambda (key)
                     (let ((variable (make-var-data meta key)))
                       (pair key (get-element 'data variable))))
                   keys)
              (begin (newline) (display "No dimesion data for ")
                     (display (get-element 'name meta))
                     (display ", adding int index")
                     (map (lambda (key val)
                            (pair key (list-tabulate val (lambda (x) x))))
                          keys vals))))
         (dimensions (filter (lambda (x)
                               (if (> (length (get-value x)) 1) #t #f))
                             loaded-dims))
         (single-dimensions (filter (lambda (x)
                               (if (< (length (get-value x)) 2) #t #f))
                        loaded-dims)))
    (add-element 'single-dimensions
                 single-dimensions
                 (add-element 'dimensions
                              dimensions
                              (tag-type 'dim-loaded variable)))))

(define (get-key a-element)
  (car a-element))
(define (get-value a-element)
  (cdr a-element))

(define (list-data->tree-data variable)
  (let* ((variable (cond ((not (dim-tagged? variable))
                          (list-data->labeled-data variable))
                         (else variable)))
         (tree-var (del-assoc 'data variable)))
    ;; much slower if you reverse the dims
    ;; (define (reverse-dim a b)
    ;;   (dim<? (reverse a) (reverse b)))
    (define (dim<? a b)
      (cond ((null? a) #f)
            ((< (car a) (car b)) #t)
            (else (dim<? (cdr a) (cdr b)))))
    (add-element 'data
                 (alist->wt-tree
                  (make-wt-tree-type
                   dim<?)
                  (get-element 'data variable))
                 (tag-type 'wt-tree tree-var))))

(define (list-data->array variable)
  ;; right now only support for two dims, but okay
  ;; because really think this will only be used
  ;; for plotting purposes
  (let* ((variable (cond ((raw? variable)
                          (add-dims variable))
                         (else variable)))
         (data (cond ((dim-tagged? variable)
                      (list->vector (map get-value
                                         (get-element 'data variable))))
                     (else (list->vector (get-element 'data variable)))))
         (array-var (del-assoc 'data variable))
         (dimensions (map get-value (get-element 'dimensions array-var))))
    (if (> (length dimensions) 2)
        (error "->array called on var with more than two dims"
               (length dimensions)))
    (let* ((first-dim (length (car dimensions)))
           (second-dim (length (cadr dimensions)))
           (start-idx (list-tabulate second-dim
                                     (lambda (x) (* first-dim x))))
           (end-idx (map (lambda (x) (+ x first-dim)) start-idx)))
      ;; generate a list of (row) vectors, each one equal to len second-dim
      ;; array, for e.g. see /kernel/iterat.scm
      (add-element 'data
                   (apply vector (map (lambda (start end)
                                              (subvector data start end))
                                            start-idx
                                            end-idx))
                   (tag-type 'array array-var)))))


(define (list-data->labeled-data variable)
  ;; takes in raw list of data and makes an alist with each key the coordinates
  (let* ((variable (if (not (assoc 'dimensions variable))
                      (add-dims variable)
                      variable))
         (dimensions (get-element 'dimensions variable))
         (dim-values (map get-value dimensions))
         (data (get-element 'data variable))
         (labelled-var (del-assoc 'data variable)))
    (define (tag-data dim-val dat)
      ;; note we still have access to dimvalues
      (let loop ((dim-val dim-val)
                 (dat dat))
        (if (null? dat)
            (if (null? (last dim-val))
                '()
                (error "dat reached null before dims - check index"
                       (list dim-val)))
            (cons (cons (map car dim-val) (car dat))
                  (loop (null-check-and-advance dim-val)
                        (cdr dat))))))
    (define (null-check-and-advance dim-val)
      ;; advance first dim location
      (let ((advanced-dim (cons (cdr (car dim-val)) (cdr dim-val))))
        (let loop ((new-dims advanced-dim)
                   (orig-dims dim-values))
          (if (null? (cdr new-dims))
              new-dims ;'()
              (let ((first (car new-dims))
                    (second (cadr new-dims)))
                (if (null? first)
                    ;; reset first and advance second
                    (cons (car orig-dims)
                          (loop (cons (cdr second) (cddr new-dims))
                                (cdr orig-dims)))
                    ;; else, advance loop
                    (cons first (loop (cdr new-dims)
                                      (cdr orig-dims)))))))))
    (add-element 'data
                 (tag-data dim-values data)
                 (tag-type 'dim-tagged labelled-var))))

(define (find-nearest val lis)
  ;; assumes list numeric and sorted small to large,
  ;; finds closest value in lis to val
  (if (< val (car lis))
      (error "val smaller than range of list"
             (list val (car lis))))
  (let loop ((li lis))
    (cond ((null? (cdr li)) (error "val larger than range of list"
                                   (list val (car li))))
          ((< val (cadr li)) (if (< (abs (- val (car li)))
                                    (abs (- val (cadr li))))
                                 (car li)
                                 (cadr li)))
          (else (loop (cdr li))))))

(define (index-data coords lab-data)
  ;; return data element closes to the given coords
  ;; from the labelled data structure
  (let ((data (get-element 'data lab-data))
        (dimensions (let ((dimensions (get-element 'dimensions lab-data)))
                      (map (lambda (key)
                             (get-element key dimensions))
                           (get-keys (get-element 'dimensions lab-data))))))
    (if (= (length dimensions) (length coords))
        (let ((exact-coords (map find-nearest coords dimensions)))
          (assoc exact-coords data))
        (error "supplied dimension of coords do not match dim. of data"
               (list coords (length dimensions))))))

(define (index-tree coords tree-data)
  (let ((data (get-element 'data tree-data))
        (dimensions (let ((dimensions (get-element 'dimensions tree-data)))
                      (map (lambda (key)
                             (get-element key dimensions))
                           (get-keys (get-element 'dimensions tree-data))))))
    (if (= (length dimensions) (length coords))
        (let ((exact-coords (map find-nearest coords dimensions)))
          `(,exact-coords . ,(wt-tree/lookup data exact-coords #f)))
        (error "supplied dimension of coords do not match dim. of data"
               (list coords (length dimensions))))))

(define (load-tree metadata var)
  (let ((raw (make-var-data metadata var)))
    (list-data->tree-data raw)))

(define (load-gdbm metadata-var)
  (let ((raw (make-var-data metadata var)))
    (list-data->gdbm raw)))

(define (list-data->gdbm variable)
  (let* ((variable (cond ((not (dim-tagged? variable))
                          (list-data->labeled-data variable))
                         (else variable)))
         (gdbm-var (del-assoc 'data variable))
         (dbf (gdbm-open "testdb.db" 65536 gdbm_wrcreat 755)))
    ;; should probably signal error on gdbm return #f (repeat key)
    (for-each (lambda (key.item)
                (gdbm-store dbf
                            (string (car key.item))
                            (number->string (cdr key.item))
                            gdbm_insert))
              (get-element 'data variable))
    (add-element 'data dbf (tag-type 'gdbm gdbm-var))))

(define (index-gdbm coords gdbm-data)
  (let ((dbf (get-element 'data gdbm-data))
        (dimensions (let ((dimensions (get-element 'dimensions gdbm-data)))
                      (map (lambda (key)
                             (get-element key dimensions))
                           (get-keys (get-element 'dimensions gdbm-data))))))
    (if (= (length dimensions) (length coords))
        (let ((exact-coords (map find-nearest coords dimensions)))
          (cons exact-coords (string->number
                              (gdbm-fetch dbf (string exact-coords)))))
        (error "supplied dimension of coords do not match dim. of data"
               (list coords (length dimensions))))))


