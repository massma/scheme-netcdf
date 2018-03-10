(declare (usual-integrations))
(load-option 'ffi)
(C-include "netcdf")

(define string (let* ((alien (make-alien-to-free
                              '(* char)
                              (lambda (retval)
                                (C-call "nc_inq_libvers" retval))))
                      (new (c-peek-cstring alien)))
                 (if (string? new)
                     new
                     (utf8->string new))))
(display string)

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

;;"/home/adam/scratch/data/isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc"
(C-include "netcdf")
(define ncid (let* ((alien-out (make-alien 'int))
                    (bytevector (string->utf8
                                 (string-append
                                  "/home/adam/scratch/data/"
                                  "isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc")))
                    ;; (chars (make-alien '(* char)))
                    (chars (malloc (1+ (* (c-sizeof "char")
                                          (bytevector-length bytevector)))
                                   '(* char)))
                    (alien-mode (malloc (c-sizeof "int") 'int))
                    (alien-ncid (make-alien '(* int))))
               (c-poke-string chars bytevector)
               (c->= alien-mode "int" 0)
               (display (c-peek-cstring chars))
               (newline)
               (display (c-> alien-mode "int"))
               (newline)
               (display "test")
               (newline)
               (display alien-out)
               (newline)
               (display chars)
               (newline)
               (display alien-ncid)
               (C-call "nc_open"
                       alien-out
                       chars
                       0 ; alien-mode
                       alien-ncid)
               (display "test")
               (if (alien-null? alien-ncid)
                   (display "eroor could't open")
                   (display "did it"))
               ;;(C-> alien-ncid "int" alien-ncid)
               (C-call "nc_close" alien-ncid)
               alien-out))

;; maybe try that alloc bytevector thing with char





