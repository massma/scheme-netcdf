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

;;;; Build and test the test plugin.
(load-option 'ffi)

(load-option 'synchronous-subprocess)
(with-working-directory-pathname
 (directory-pathname (current-load-pathname)) ; (pwd) - if from repl
 (lambda ()
   (let ((port (notification-output-port)))
     (fresh-line port)
     (write-string "./autobuild.sh in src/netcdf/" port)
     (newline port))
   (let ((status (run-synchronous-subprocess "sh" '("./autobuild.sh"))))
     (if (not (zero? status))
         (error "netcdf build failed:" status)))))


(load "test-netcdf")
