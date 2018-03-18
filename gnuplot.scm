;;; This file is part of Adaptive Plot, a library for intelligently
;;; plotting functions from the MIT Scheme REPL.
;;; Copyright (C) 2010-2011, 2013 Alexey Radul
;;; Modifications for scheme-netcdf (C) 2018, Adam Massmann
;;;
;;; Adaptive Plot is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; Adaptive Plot is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with Adaptive Plot.  If not, see
;;; <http://www.gnu.org/licenses/>.

;; (load-option 'ffi)
;; (c-include "gnuplot_utils")
(declare (usual-integrations))

;;;; Gnuplot output of alist data

;;; This is just writing files to disk and issuing shell commands.
;;; This file intentionally does not depend on any of the rest of
;;; Adaptive Plot, so can be lifted and used elsewhere.

(load-option 'synchronous-subprocess)
(load-option 'subprocess)

(define (gnuplot-write-alist alist filename)
  (with-output-to-file filename
    (lambda ()
      (for-each
       (lambda (x.y)
	 (write (exact->inexact (car x.y)))
	 (write-string " ")
	 (write (exact->inexact (cdr x.y)))
	 (newline))
       alist))))

;; (define (write-c float)
;;   (let ((alien (malloc (c-sizeof "float") 'float)))
;;     (c->= alien "float" float)
;;     (c-call "write_var" alien)))


(define (round-off n)
  (let ((power (expt 10 n)))
    (lambda (z)
      (/ (round (* power z)) power))))

(define (rounder num)
  (/ (round (* 100 num)) 100))

(define (write-key key port)
  (write (exact->inexact (rounder key)) port)
  (write-char #\space port)
  ;;(write-c (exact->inexact key))
  )

;; (define (with-output-to-binary-file pathname thunk)
;;   (call-with-binary-output-file
;;    pathname
;;    (lambda (port)
;;      (parameterize* (cons (cons current-output-port port) '()) thunk))))

(define (gnuplot-write-wt-tree wt-tree row-ender? port)
  ;;with-output-to-binary-file filename
  (wt-tree/for-each
   (lambda (keys value)
     (for-each (lambda (key) (write-key key port)) keys)
     (write (exact->inexact (rounder value)) port)
     (write-char #\newline port)
     (if (row-ender? keys)
         (write-char #\newline port))
     ;;(write-c (exact->inexact value))
     )
   wt-tree))

(define (gen-row-ender dimensions)
  ;; maybe to handle single dim files put if single dim return #f
  (let ((eq-vals (map last (map get-value dimensions))))
    (lambda (keys)
      (let loop ((dims (cdr keys))
                 (eq-vals (cdr eq-vals)))
        (cond ((null? dims)
               #t)
              ((equal? (car dims) (car eq-vals))
               (loop (cdr dims) (cdr eq-vals)))
              (else #f))))))

(define (write-vector row)
  (vector-for-each (lambda (element)
                     (write (exact->inexact element))
                     (write-string " "))
                   row))
(define (gnuplot-write-array array filename)
  (with-output-to-file filename
    (lambda ()
      (vector-for-each
       (lambda (row)
         (write-vector row)
         (newline))
       array))))

(define (gnuplot-cmap variable dimmap)
  ;; should add functionality where you acn give a dim
  ;; mpa and push and pull
  (if (not (wt-tree? variable))
      (error (string-append "gnuplot-cmap called on variable other than"
                            "wt-tree. currently, only wt-tree plotting"
                            "is supported")
             (get-element 'type variable)))
  (let* ((process (start-pipe-subprocess
                 "/bin/sh"
                 (list->vector
                  (cons "/bin/sh" (os/form-shell-command "gnuplot -p")))
                 #f)))
    (parameterize ((current-output-port (subprocess-input-port process)))
      (map (lambda (string) (write-string string (current-output-port)))
           (list "set view map\n"
                 "set datafile missing \"#[NaN]\"\n"
                 "splot \"-\" using 1:2:3 with image\n"))
      (gnuplot-write-wt-tree (get-element 'data variable)
                             (gen-row-ender (get-element 'dimensions variable))
                             (current-output-port))
      (write "exit\n" (current-output-port))
      (flush-output-port (current-output-port))
      (subprocess-quit process)
      (subprocess-delete process))))

;; A "lax alist" is a list whose pairs are treated as alist elements,
;; but which is allowed to have non-pairs also (which are ignored).
(define (lax-alist-lookup alist item default #!optional =)
  (let ((binding (assoc item (filter pair? alist) =)))
    (if binding
        ;; I really want to be looking up from two-element lists
        ;; rather than pairs, so this does not iterpret proper alists.
        (cadr binding)
        default)))

;; below worked, not you als omight not need to flush manually
;; when using this in practice, as buffering should flush when
;; the output starts getting full. should test performance of
;; manual flushing.
