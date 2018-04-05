(cd "/home/adam/software/netcdf")
(load "install-netcdf")
(load-option 'ffi)
(C-include "netcdf")
(cf "netcdf")
(load "netcdf")
(load "./test-manager/load")


(parameterize ((working-directory-pathname "./testing"))
  (run-shell-command "make gen-version.exe")
  (run-shell-command "make simple_xy_nc4.nc"))


(define nc-filename "./testing/simple_xy_nc4.nc")

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
  (let* ((meta (make-meta nc-filename))
         (var (make-var-data meta "data"))
         (data (get 'data var))
         (dimensions (get 'dimensions var)))
    (check (equal? data (list->vector (list-tabulate 72 (lambda (x) x))))
           "loaded data unequal to expected values")
    (check (equal? (get "x" dimensions)
                   (list->vector (list-tabulate 6 (lambda (x) x))))
           "loaded dimensions unequal to expected values")
    (check (equal? (get "y" dimensions)
                   (list->vector (list-tabulate 12 (lambda (x) x))))
           "loaded dimensions unequal to expected values")
    (let* ((sliced-var (index '(all 3) var))
          (data (get 'data sliced-var))
          (dimensions (get 'dimensions sliced-var)))
      (check (equal? data (list->vector (list-tabulate 6 (lambda (x)
                                                           (+ 3 (* x 12))))))
             "all-sliced data unequal to expected values")
      (check (equal? dimensions
                     (pair "x" (list->vector
                                (list-tabulate 6 (lambda (x) x)))))
             "all-sliced dimensions unequal to expected values")
      (check (equal? (get 'single-dimensions sliced-var)
                     (pair "y" (list->vector (list 3))))
             "all-sliced single dimensions unequal to expected values")
      (let* ((sliced-var (index (list 2) sliced-var))
             (data (get 'data sliced-var))
             (dimensions (get 'dimensions sliced-var)))
        (check (equal? data (list->vector
                             (list-tabulate 1 (lambda (x)
                                                (+ 3 (* (+  x 2) 12))))))
               "slice of single dim data unequal to expected values")
        (check (null? dimensions)
               "slice of single dim not null")
        (check (equal? (get 'single-dimensions sliced-var)
                       (list (pair "x" (list->vector (list 2)))
                             (pair "y" (list->vector (list 3)))))
               "slice of single dim wrong single dimensions")))
    (let* ((sliced-var (index '((2 . 4) 4) var))
          (data (get 'data sliced-var))
          (dimensions (get 'dimensions sliced-var)))
      (check (equal? data (list->vector
                           (list-tabulate 3 (lambda (x)
                                              (+ 4 (* (+ x 2) 12))))))
             "sliced data unequal to expected values")
      (check (equal? dimensions
                     (pair "x" (list->vector
                                (list-tabulate 3 (lambda (x) (+ 2 x))))))
             "sliced dimensions unequal to expected values")
      (check (equal? (get 'single-dimensions sliced-var)
                     (pair "y" (list->vector (list 4))))
             "sliced single dimensions unequal to expected values"))
    (let* ((sliced-var (index '(1 5) var))
          (data (get 'data sliced-var))
          (dimensions (get 'dimensions sliced-var)))
      (check (equal? data (list->vector
                           (list-tabulate 1 (lambda (x)
                                              (+ 5 (* (+ x 1) 12))))))
             "indexed data unequal to expected value")
      (check (null? dimensions)
             "indexed dimensions unequal to expected values")
      (check (equal? (get 'single-dimensions sliced-var)
                     (list (pair "x" (list->vector (list 1)))
                           (pair "y" (list->vector (list 5)))))
             "indexed single dimensions unequal to expected values"))))


(run-registered-tests)
(cd "../")

