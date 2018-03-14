(cd "/home/adam/software/netcdf")
(load-option 'ffi)
(C-include "netcdf")
(cf "netcdf")
;; (load "install-netcdf")
(load "netcdf")
(load "./test-manager/load")

(cd "./testing")

(define nc-filename "./simple_xy_nc4.nc")
(run-shell-command "make gen-version.exe")
(run-shell-command "make simple_xy_nc4.nc")

;; test version
(define-test (version)
  (assert-equal (let* ((alien (make-alien
                               '(* char))))
                  (C-call "nc_inq_libvers" alien)
                  (let ((new (c-peek-cstring alien)))
                    (if (alien-null? alien)
                        (error "Could not open-file."))
                    (if (string? new)
                        new
                        (utf8->string new))))
                (call-with-output-string (lambda (port)
                                           (run-shell-command
                                            "./gen-version.exe"
                                            'output port)))
         "scheme netcdf-version incorrect"))

(define-test (simple-xy)
  (assert-equal (let* ((meta (make-meta nc-filename))
                       (data (get-element 'data (make-var-data meta "data"))))
                  (close-ncid meta)
                  data)
                (list-tabulate 72 (lambda (x) x)))
  "var data loaded doesn't match generated")

(run-registered-tests)
(cd "../")

;; (define data
;;   (let* ((metadata (make-meta
;;                     (string-append
;;                      "/home/adam/scratch/data/"
;;                      "isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc"))))
;;     (define data (with-timings
;;                   (lambda () (make-var-data metadata "irwin_cdr"))
;;                   (lambda (run-time gc-time real-time)
;;                     (newline) (display "run time: ")
;;                     (write (internal-time/ticks->seconds run-time))
;;                     (write-char #\space) (newline) (display "gc time: ")
;;                     (write (internal-time/ticks->seconds gc-time))
;;                     (write-char #\space) (newline) (display "wall time: ")
;;                     (write (internal-time/ticks->seconds real-time))
;;                     (newline))))
;;     (close-ncid metadata)
;;     data))




;; (pp (get-element 'meta data))
;; (list-ref (get-element 'data data) 1949924)
;; (pa list-ref)
;; (let* ((meta (make-meta nc-filename))
;;                        (data (make-var-data meta "data")))
;;                   (close-ncid meta)
;;                   data)
