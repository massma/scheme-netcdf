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

(load-option 'ffi)
(c-include "netcdf")
(load "netcdf")
(load "test-manager/load")

(define nc-2d-path "./testing/simple_xy_nc4.nc")
(define nc-3d-path "./testing/simple_xyz_nc4.nc")

;; test version
(define-test (version)
  (assert-equal (let* ((alien (make-alien
                               '(* char))))
                  (c-call "nc_inq_libvers" alien)
                  (let ((new (c-peek-cstring alien)))
                    (if (alien-null? alien)
                        (error "Could not open-file."))
                    (if (string? new)
                        new
                        (utf8->string new))))
                (call-with-output-string (lambda (port)
                                           (run-shell-command
                                            "./testing/gen-version.exe"
                                            'output port)))
         "scheme netcdf-version incorrect"))


(define-test (simple-xy)
  (let* ((meta (make-meta nc-2d-path))
         (var (make-var-data meta "data"))
         (data (get var 'data))
         (dimensions (get var 'dimensions)))
    (check (equal? (index var (list 'all 3))
                   (index var (list (cons "y" 3)))))
    (check (equal? data (list->vector (list-tabulate 72 (lambda (x) x))))
           "loaded data unequal to expected values")
    (check (equal? (get dimensions "x")
                   (list->vector (list-tabulate 6 (lambda (x) x))))
           "loaded dimensions unequal to expected values")
    (check (equal? (get dimensions "y")
                   (list->vector (list-tabulate 12 (lambda (x) x))))
           "loaded dimensions unequal to expected values")
    (let* ((sliced-var (index var '(all 3)))
          (data (get sliced-var 'data))
          (dimensions (get sliced-var 'dimensions)))
      (check (equal? data (list->vector (list-tabulate 6 (lambda (x)
                                                           (+ 3 (* x 12))))))
             "all-sliced data unequal to expected values")
      (check (equal? dimensions
                     (pair "x" (list->vector
                                (list-tabulate 6 (lambda (x) x)))))
             "all-sliced dimensions unequal to expected values")
      (check (equal? (get sliced-var 'single-dimensions)
                     (pair "y" (list->vector (list 3))))
             "all-sliced single dimensions unequal to expected values")
      (let* ((sliced-var (index sliced-var (list 2)))
             (data (get sliced-var 'data))
             (dimensions (get sliced-var 'dimensions)))
        (check (equal? data (list->vector
                             (list-tabulate 1 (lambda (x)
                                                (+ 3 (* (+  x 2) 12))))))
               "slice of single dim data unequal to expected values")
        (check (null? dimensions)
               "slice of single dim not null")
        (check (equal? (get sliced-var 'single-dimensions)
                       (list (pair "x" (list->vector (list 2)))
                             (pair "y" (list->vector (list 3)))))
               "slice of single dim wrong single dimensions")))
    (let* ((sliced-var (index var '((2 . 4) 4)))
          (data (get sliced-var 'data))
          (dimensions (get sliced-var 'dimensions)))
      (check (equal? data (list->vector
                           (list-tabulate 3 (lambda (x)
                                              (+ 4 (* (+ x 2) 12))))))
             "sliced data unequal to expected values")
      (check (equal? dimensions
                     (pair "x" (list->vector
                                (list-tabulate 3 (lambda (x) (+ 2 x))))))
             "sliced dimensions unequal to expected values")
      (check (equal? (get sliced-var 'single-dimensions)
                     (pair "y" (list->vector (list 4))))
             "sliced single dimensions unequal to expected values"))
    (let* ((sliced-var (index var '(1 5)))
          (data (get sliced-var 'data))
          (dimensions (get sliced-var 'dimensions)))
      (check (equal? data (list->vector
                           (list-tabulate 1 (lambda (x)
                                              (+ 5 (* (+ x 1) 12))))))
             "indexed data unequal to expected value")
      (check (null? dimensions)
             "indexed dimensions unequal to expected values")
      (check (equal? (get sliced-var 'single-dimensions)
                     (list (pair "x" (list->vector (list 1)))
                           (pair "y" (list->vector (list 5)))))
             "indexed single dimensions unequal to expected values"))))

(define-test (simple-xyz)
  (let* ((meta (make-meta nc-3d-path))
         (var (make-var-data meta "data"))
         (data (get var 'data))
         (dimensions (get var 'dimensions)))
    (equal? (index var '(all 0 0))
            (index (index var '(all all 0))
                   '(all 0)))))

(run-registered-tests)


;; (define nc-2d-path "/home/adam/software/netcdf/testing/simple_xy_nc4.nc")
;; (let* ((meta (make-meta nc-2d-path))
;;        (var (make-var-data meta "data"))
;;        (data (get var 'data))
;;        (dimensions (get var 'dimensions)))
;;   (equal? (index var (list 'all 3))
;;           (index var (list (cons "y" 3))))
;;   (equal? data (list->vector (list-tabulate 72 (lambda (x) x))))
;;   "loaded data unequal to expected values"
;;   (equal? (get dimensions "x")
;;           (list->vector (list-tabulate 6 (lambda (x) x))))
;;   "loaded dimensions unequal to expected values"
;;   (equal? (get dimensions "y")
;;           (list->vector (list-tabulate 12 (lambda (x) x))))
;;   "loaded dimensions unequal to expected values"
;;   (let* ((sliced-var (index var '(all 3)))
;;          (data (get sliced-var 'data))
;;          (dimensions (get sliced-var 'dimensions)))
;;     (equal? data (list->vector (list-tabulate 6 (lambda (x)
;;                                                   (+ 3 (* x 12))))))
;;     "all-sliced data unequal to expected values"
;;     (equal? dimensions
;;             (pair "x" (list->vector
;;                        (list-tabulate 6 (lambda (x) x)))))
;;     "all-sliced dimensions unequal to expected values"
;;     (equal? (get sliced-var 'single-dimensions)
;;             (pair "y" (list->vector (list 3))))
;;     "all-sliced single dimensions unequal to expected values"
;;     (let* ((sliced-var (index sliced-var (list 2)))
;;            (data (get sliced-var 'data))
;;            (dimensions (get sliced-var 'dimensions)))
;;       (equal? data (list->vector
;;                     (list-tabulate 1 (lambda (x)
;;                                        (+ 3 (* (+  x 2) 12))))))
;;       "slice of single dim data unequal to expected values"
;;       (null? dimensions)
;;       "slice of single dim not null"
;;       (equal? (get sliced-var 'single-dimensions)
;;               (list (pair "x" (list->vector (list 2)))
;;                     (pair "y" (list->vector (list 3)))))
;;       "slice of single dim wrong single dimensions"))
;;   (let* ((sliced-var (index var '((2 . 4) 4)))
;;          (data (get sliced-var 'data))
;;          (dimensions (get sliced-var 'dimensions)))
;;     (equal? data (list->vector
;;                   (list-tabulate 3 (lambda (x)
;;                                      (+ 4 (* (+ x 2) 12))))))
;;     "sliced data unequal to expected values"
;;     (equal? dimensions
;;             (pair "x" (list->vector
;;                        (list-tabulate 3 (lambda (x) (+ 2 x))))))
;;     "sliced dimensions unequal to expected values"
;;     (equal? (get sliced-var 'single-dimensions)
;;             (pair "y" (list->vector (list 4))))
;;     "sliced single dimensions unequal to expected values")
;;   (let* ((sliced-var (index var '(1 5)))
;;          (data (get sliced-var 'data))
;;          (dimensions (get sliced-var 'dimensions)))
;;     (equal? data (list->vector
;;                   (list-tabulate 1 (lambda (x)
;;                                      (+ 5 (* (+ x 1) 12))))))
;;     "indexed data unequal to expected value"
;;     (null? dimensions)
;;     "indexed dimensions unequal to expected values"
;;     (equal? (get sliced-var 'single-dimensions)
;;             (list (pair "x" (list->vector (list 1)))
;;                   (pair "y" (list->vector (list 5)))))
;;     "indexed single dimensions unequal to expected values"))
