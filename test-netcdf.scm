(load-option 'ffi)
(C-include "netcdf")
(cf "netcdf")
;; (load "install-netcdf")
(load "netcdf")

(define data
  (let* ((metadata (make-meta
                    (string-append
                     "/home/adam/scratch/data/"
                     "isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc"))))
    (define data (with-timings
                  (lambda () (make-var-data metadata "irwin_cdr"))
                  (lambda (run-time gc-time real-time)
                    (newline) (display "run time: ")
                    (write (internal-time/ticks->seconds run-time))
                    (write-char #\space) (newline) (display "gc time: ")
                    (write (internal-time/ticks->seconds gc-time))
                    (write-char #\space) (newline) (display "wall time: ")
                    (write (internal-time/ticks->seconds real-time))
                    (newline))))
    (close-ncid metadata)
    data))



(pp (get-element 'meta data))
(list-ref (get-element 'data data) 1949924)
(pa list-ref)
;;(define vec-list (alien-array->list alien-var nelements (lambda (x) x)))
;; (define nan-filtered (filter (lambda (x) (not (flo:nan? x)))
;;                            vec-list))
;; (define inf-filtered (filter flo:finite?
;;                              nan-filtered))


;; (define two00s (filter (lambda (x) (eqv? 200.0 x)) inf-filtered))
;; (let* ((ntotal (length vec-list))
;;        (nnan (- ntotal (length nan-filtered)))
;;        (noutside (- ntotal nnan (length inf-filtered))))
;;   (newline) (display "number of nans: ") (display (inexact (/ nnan ntotal)))
;;   (newline) (display "number ouside of range: ")
;;   (display (inexact (/ noutside ntotal)))
;;   (newline) (display "number obbs=200.0: ") (display (length two00s)))

;; test metadata
;; (define meta (make-meta
;;               (string-append "/home/adam/scratch/data/"
;;                              "isccp/b1/GRIDSAT-B1.1987.05.03.18.v02r01.nc")))
;; (display (get-filepath meta))
;; (define dims (get-dimensions meta))
;; (define vars (get-variables meta))
;; (define attrs (get-attributes meta))
;; (pp load-var-meta)
