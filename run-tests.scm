(load-option 'ffi)
(load "/home/adam/software/test-manager/load")
(load "build-netcdf")

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
;; should print some value
