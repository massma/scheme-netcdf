#| -*-Scheme-*-

Copyright (C) 2018 Adam Massmann <massmannak@gmail.com>

This file is part of scheme-netcdf.

scheme-netcdf is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3 of the License, or (at
your option) any later version.

scheme-netcdf is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with scheme-netcdf; if not, see <http://www.gnu.org/licenses/>

|#

(declare (usual-integrations))
(load-option 'ffi)
(c-include "netcdf")


;;; Below are functions designed to be used by user
(define (make-meta filename)
  (define (string-splitter delimiter char copy boolean)
    ;; function for stable branch compatability, can be deleted on next version
    ;; or switch to dev branch delimter, copy and boolean are dummy args
    (define (looper string)
      (let ((index (string-find-next-char string char)))
        (if index
            (cons (substring string 0 index)
                  (looper (string-tail string (+ index 1))))
            '())))
    looper)
  (define (string-joiner infix char)
    ;; function for stable branch compatability, can be deleted on next version
    ;; or switch to dev branch delimter, copy and boolean are dummy args
    (define (joiner . list)
      (if (null? list)
          ""
          (string-append (car list) char (apply joiner (cdr list)))))
    joiner)
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
  ;; file-level metadata (using ncdump rather than c ffi)
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

(define (make-var-data meta name #!optional post-processing)
  (define (load-var-data var-meta)
    (define (build-processor var-meta)
      ;; makes a function for converted stored values to float,
      ;; i.e. if we have scale factors and offsets, etc.
        (let* ((attrs (get var-meta 'atts))
               (fillv (if (assoc "_FillValue" attrs)
                          (get attrs "_FillValue")
                          (begin (newline)
                                 (display "WARNING - no fill value")
                                 nan)))
               (offset (if (assoc "add_offset" attrs)
                           (get attrs "add_offset")
                           0))
               (scale (if (assoc "scale_factor" attrs)
                          (get attrs "scale_factor")
                          1))
               (valid_range (if (assoc "valid_range" attrs)
                                (get attrs "valid_range")
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
    
    (let* ((ncid (get var-meta 'ncid))
           (varid (get var-meta 'varid))
           (nelements (apply * (alist->list (get-list var-meta 'dims))))
           (type (get var-meta 'xtype))
           (alien-var (malloc (* nelements (get-c-sizeof type))
                              (string->symbol type)))
           (out (c-call "nc_get_var" ncid varid alien-var)))
      (cond ((= 0 out))  ; (newline) (display "loading var sucessful")
            ((= -49 out) (error "Variable not found" varid))
            ((= -60 out) (error "Math result not representable" varid))
            ((= -39 out) (error "Operation not allowed in define mode" varid))
            ((= -33 out) (error "Not a netcdf id" varid))
            ((alien-null? alien-ncid)
             (error "could't load var- unspecified reason" varid))
            (else (error "unspecified response")))
      (alien->vector alien-var nelements type (build-processor var-meta))))

  (define (add-dims variable)
  (let* ((meta (get variable 'meta))
         (ncid (get meta 'ncid))
         (dims (get meta 'dims))
         (keys (get-keys dims))
         (vals (alist->list dims))
         (loaded-dims
          (if (and-list (map (lambda (key)
                               (var-exists? ncid key))
                             keys))
              (map (lambda (key)
                     (pair key (make-var-data
                                meta key (lambda (var) (get var 'data)))))
                   keys)
              (begin (newline) (display "No dimesion data for ")
                     (display (get meta 'name))
                     (display ", adding int index")
                     (map (lambda (key val)
                            (pair key
                                  (list->vector
                                   (list-tabulate val (lambda (x) x)))))
                          keys vals))))
         (dimensions (filter (lambda (x)
                               (> (vector-length (get-value x)) 1))
                             loaded-dims))
         (single-dimensions (filter (lambda (x)
                                      (< (vector-length (get-value x)) 2))
                                    loaded-dims)))
    (add-element 'single-dimensions
                 single-dimensions
                 (add-element 'dimensions
                              dimensions
                              variable))))
  (let* ((var-meta (load-var-meta meta name))
         (var-data (load-var-data var-meta))
         (post-processing (if (default-object? post-processing)
                              add-dims
                              post-processing)))
    (post-processing (list (pair 'data var-data)
                           (pair 'meta var-meta)
                           (pair 'shape (alist->list
                                         (get-list var-meta 'dims)))))))

(define (get structure key)
  (let ((value (assoc key structure)))
    (if value
        (let ((output (cdr value)))
          (if (equal? (length output) 1)
              (car output)
              output))
        (error "key not in structure" (list key (map get-key structure))))))

(define (index variable unformat-coords)
  ;; added below to allow for named coordinates
  (define (named? coords)
    ;; note below does not allow for string coordinates, but our indexing
    ;; doesn't really either
    (if (pair? (car coords))
        (string? (car (car coords)))
        (string? (car coords))))
  (define coords (if (named? unformat-coords)
                     (map (lambda (dim-key)
                            (if (assoc dim-key unformat-coords)
                                (cdr (assoc dim-key unformat-coords))
                                'all))
                          (get-keys (get-list variable 'dimensions)))
                     unformat-coords))
  ;; internal func
  (define (calc-index index-list shape)
      (let ((rev-index (reverse index-list))
            (rev-shape (reverse shape)))
        (fix:+ (car rev-index)
               (let loop ((index (cdr rev-index))
                          (shape rev-shape)
                          (mult 1))
                 (if (null? index)
                     0
                     (fix:+
                      (fix:* (car index) (fix:* mult (car shape)))
                      (loop (cdr index)
                            (cdr shape)
                            (fix:* mult (car shape)))))))))
  (define (slice-dimension new-dims variable)
    (let* ((idxs (map cadr new-dims))
           (data (get variable 'data))
           (shape (get variable 'shape))
           (new-shape (map length idxs))
           (new-length (apply * new-shape))
           (new-vec (make-vector new-length nan)))
      (define (advance-index-list old-idxs)
      (let ((reverse-idx (reverse old-idxs)))
        (let ((advanced-index
               (cons (cdr (car reverse-idx))
                     (cdr reverse-idx))))
          (reverse
           (let loop ((new-index advanced-index)
                      (orig-index (reverse idxs)))
             (if (null? (cdr new-index))
                 new-index              ;'()
                 (let ((first (car new-index))
                       (second (cadr new-index)))
                   (if (null? first)
                       ;; reset first and advance second
                       (cons (car orig-index)
                             (loop (cons (cdr second) (cddr new-index))
                                   (cdr orig-index)))
                       ;; else, advance loop
                       (cons first (loop (cdr new-index)
                                         (cdr orig-index)))))))))))
      (let loop-idx ((i 0)
                     (index-list idxs)
                     (loop-list idxs)
                     (indices '()))
        (if (fix:= i new-length)
            new-vec
            (if (null? loop-list)
                (let ((adv-idx (advance-index-list index-list)))
                  (vector-set! new-vec i
                               (vector-ref data (calc-index indices shape)))
                  (loop-idx (fix:+ i 1) adv-idx adv-idx '()))
                (loop-idx i index-list (cdr loop-list)
                          (append indices (list (car (car loop-list))))))))))
  (define (build-dimension new-dims dimensions)
    ;; new-dims is a list of 2-element lists, frist vec of dims dims,
    ;; second list of indexes
    ;; dimensions is an alist of key, value pairs
    ;; outputs new alist of key, new dimensions pairs
    (let ((dim-keys (get-keys dimensions))
          (new-vectors (map car new-dims)))
      (map pair dim-keys new-vectors)))
  (define (slice-vector val vec)
      ;; assumes list numeric and sorted small to large,
      ;; finds closest value in lis to val
      (if (not (pair? val)) (error "slice should be defined by pair" val))
      (let ((length (vector-length vec))
            (min-val (car val))
            (max-val (cdr val)))
        (let loop ((i 0)
                   (idx-list '())
                   (val-list '()))
          (if  (fix:= length i)
               (list (list->vector val-list) idx-list)
               (let ((current-val (vector-ref vec i)))
                 (if (and (>= current-val min-val) (<= current-val max-val))
                     (loop (fix:+ i 1)
                           (append idx-list (list i))
                           (append val-list (list current-val)))
                     (loop (fix:+ i 1) idx-list val-list)))))))
  (define (gen-new-dims val dimension)
    ;; takes a list of coordinates (do not need to be exact
    ;; outputs a list two elements: vector of exact coordinates,
    ;; and index of list
    (let ((vec (get-value dimension)))
      (cond ((pair? val) (slice-vector val vec))
            ((equal? val 'all) (list vec (list-tabulate (vector-length vec)
                                                        (lambda (x) x))))
            ;; should modify below so idx in list
            ((number? val) (select-coords val vec)) 
            (else (error "invalid slice")))))

  (define (select-coords val vec)
      ;; assumes list numeric and sorted small to large,
      ;; finds closest value in lis to val
      (define (between? value compare1 compare2)
        (or (and (>= value compare1) (<= value compare2))
            (and (<= value compare1) (>= value compare2))))
      (let ((length (vector-length vec)))
        (let loop ((i 0))
          (if (fix:= (fix:- length 1) i)
              (error "val not between any dimensions"
                     (list val
                           (vector-ref vec 0)
                           (vector-ref vec (fix:- length 1))))
              (let ((cur-val (vector-ref vec i))
                    (next-val (vector-ref vec (fix:+ i 1))))
                (if (between? val cur-val next-val)
                    (if (< (abs (- val cur-val))
                           (abs (- val next-val)))
                        (list (vector cur-val)
                              (list i))
                        (list (vector next-val)
                              (list (fix:+ i 1))))
                    (loop (fix:+ i 1))))))))
  ;; return data element closes to the given coords
  ;; from the labelled data structure
  (let ((dimensions (get-list variable 'dimensions))
        (single-dims (get-list variable 'single-dimensions))
        (new-var (del-assoc
                  'dimensions
                  (del-assoc 'data
                             (del-assoc 'single-dimensions
                                        (del-assoc 'shape variable))))))
    (if (= (length dimensions) (length coords))
        (let* ((new-dims (map gen-new-dims coords dimensions))
               (dims (build-dimension new-dims dimensions))
               (new-var (add-element
                         'dimensions
                         (filter (lambda (x)
                                   (> (vector-length (get-value x)) 1))
                                 dims)
                         (add-element 'data
                                      (slice-dimension new-dims variable)
                                      (add-element 'shape
                                                   (map vector-length
                                                        (get-values dims))
                                                   new-var))))
               (new-single-dims (filter (lambda (x)
                                          (< (vector-length (get-value x)) 2))
                                        dims)))
          (add-element 'single-dimensions
                       (append new-single-dims single-dims)
                       new-var))
        (error "supplied dimension of coords do not match dim. of data"
               (list coords (length dimensions))))))

;;; internal access/creators
(define (pair key value)
  (if (not (list? value))
      (cons key (list value))
      (cons key value)))

(define (add-element key value structure)
  (cons (pair key value) structure))

(define (get-list structure key)
  ;; differs from above in that gaurantees to send a list
  ;; e.g. if you need it for map on length = 1 elements
  (let ((value (assoc key structure)))
    (if value
        (cdr value)
        (error "key not in structure" (list key (map get-key structure))))))

(define (get-keys structure)
  (map get-key structure))

(define (get-values structure)
  (map (lambda (key) (get structure key)) (get-keys structure)))

(define (get-key a-element)
  (car a-element))

(define (get-value a-element)
  (cadr a-element))

(define (alist->list structure)
  (map (lambda (x) (get-value x)) structure))

(define (key-exists? key structure)
  (assoc key structure))

(define (var-exists? ncid var-name)
  (let* ((alien-varid (malloc (c-sizeof "int") 'int))
         (out (C-call "nc_inq_varid" ncid (->cstring var-name) alien-varid)))
    (cond ((= -33 out) (error "Bad netcdf id on test-var" ncid))
          ((= 0 out) #t)
          (else #f))))

(define (and-list x)
  ;; returns true if every element of list is #t
  (if (null? x)
      #t
      (if (car x)
          (and-list (cdr x))
          #f)))

;; load functions
(define (close-ncid metadata)
  ;; this is dangerous, introduces state, :(, run when done w/ file
  (let* ((ncid (get metadata 'ncid))
         (out (C-call "nc_close" ncid)))
    (cond ((= 0 out) (newline) (display "closing sucessful"))
          ((= -33 out) (error "Not a netcdf id." ncid))
          ((= -116 out) (error "Bad group ID." ncid))
          (else (error "unspecified response")))
    #f))


;; below could be useful for users; if big data might not want whole thing
(define (load-var-meta metadata varname)
  (define (load-varid metadata var-name)
    (let* ((alien-varid (malloc (c-sizeof "int") 'int))
           (ncid (get metadata 'ncid))
           (out (C-call "nc_inq_varid" ncid
                        (->cstring var-name) alien-varid)))
      (cond ((= 0 out) (newline) (display "loading varid sucessful"))
            ((= -33 out) (error "Not a netcdf id." ncid))
            ((alien-null? alien-varid)
             (error "could't open file-unspecified reason" ncid))
            (else (error "unspecified response")))
      (C-> alien-varid "int")))
  (define (load-dim-meta ncid dimid)
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
  (define (make-dim-meta var-meta)
    (if (key-exists? 'dims var-meta)
        (error "Called make-dim-meta but dims already exists" var-meta)
        (add-element
         'dims
         (map (lambda (dimid)
                (load-dim-meta (get var-meta 'ncid) dimid))
              (get-list var-meta 'dimids))
         var-meta)))

  (define (load-att-names var-meta)
    (define (load-attname ncid varid attnum)
      ;; ony loads single att name
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
    ;; get each name of att for loading by load-att
    (map (lambda (attnum)
           (load-attname (get var-meta 'ncid)
                         (get var-meta 'varid)
                         attnum))
         (list-tabulate (get var-meta 'natts) (lambda (x) x))))

  (define (load-att var-meta name)
    ;; load and build attributes
    (let* ((ncid (get var-meta 'ncid))
           (varid (get var-meta 'varid))
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
                          (list->string
                           (map ascii->char
                                (alien->list alien-data nelements type)))
                          (alien->list alien-data nelements type)))))))
        (pair name (loader)))))

  (define (make-var-structure metadata varid alien-name alien-xtype
                              alien-ndims alien-dimids alien-natts)
    (let ((ndims (c-> alien-ndims "int") ))
      `((filename . (,(get metadata 'filename)))
        (ncid . (,(get metadata 'ncid)))
        (name . (,(alien->string alien-name)))
        (varid . (,varid))
        (xtype . (,(alien->type alien-xtype)))
        (ndims . (,ndims))
        (dimids . ,(alien->list alien-dimids ndims "int"))
        (natts . (,(c-> alien-natts "int"))))))
  
  (let* ((ncid (get metadata 'ncid))
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
    (cond ((= 0 out))     ;(newline) (display "loading var sucessful")
          ((= -49 out) (error "Variable not found" varid))
          ((= -33 out) (error "Not a netcdf id" varid))
          ((alien-null? alien-ndims)
           (error "could't load var- unspecified reason" varid))
          (else (error "unspecified response")))
    (let* ((var-meta
            (make-dim-meta
             (make-var-structure
              metadata varid alien-name alien-xtype
              alien-ndims alien-dimids alien-natts)))
           (att-names (load-att-names var-meta)))
      ;; add atts to var-meta,
      ;; and return the metadata structure
      (add-element 'atts
                   (map (lambda (name)
                          (load-att var-meta name))
                        att-names)
                   var-meta))))

;; alien/c functions and converters
(define (->cstring string)
  ;; lifted from x11-base.scm
  (cond ((and (integer? string) (zero? string))
         0)
        ((string? string)
         ;; String->iso8859-1 would be incorrect; it does not null terminate.
         (let* ((end (string-length string))
                (result (make-vector-8b (fix:1+ end))))
           (do ((i 0 (fix:1+ i)))
               ((not (fix:< i end))
                (vector-8b-set! result i 0))
             (vector-8b-set! result i (char->integer
                                       (string-ref string i))))
           result))
        (else
         (error:wrong-type-argument string "a string or 0" '->cstring))))

(define (alien->vector alien nelements type process)
  ;; convert alien array to vector of lenth D*D*D...
  (let ((vec (make-vector nelements nan))
        (peek (get-peek type))
        (advance (get-advance type)))
    ;; below using cons maximum recursion depth is exceeded
    (let loop ((n 0))
      (if (< n nelements)
          (let ((value (peek alien)))
            (vector-set! vec n (process value))
            (advance alien)
            (loop (1+ n)))))
    vec))

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
  ;; convert alien to string
  (let ((new (c-peek-cstring alien)))
    (if (alien-null? alien)
        (error "No string in pointer."))
    (if (string? new)
        new
        (utf8->string new))))

(define (alien->type alien)
  ;; convert netcdf integer type flag to type string
  (let ((type (c-> alien "int")))
    (cdr (assoc type +type-conv-key+))))


;;;; attribute funtions
(define +type-conv-key+ '((0 . "not a type") (1 . "byte") (2 . "char")
                          (3 . "short") (4 . "int") (5 . "float")
                          (6 . "double") (7 . "ubyte") (8 . "ushort")
                          (9 . "uint") (10 . "uint64") (11 . "uint64")
                          (12 . "string")))

;; c function accessors
(define (get-peek type)
  ;; returns c-peek function from type str
  (cdr (assoc type +peek-list+)))

(define (get-advance type)
  ;; returns c-advance-array-loc func from type str
  (cdr (assoc type +advance-list+)))

(define (get-c-sizeof type)
  ;; returns c-size of from type str
  (cdr (assoc type +size-list+)))

;;; constants
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



;;; Useful IEEE defined values
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

