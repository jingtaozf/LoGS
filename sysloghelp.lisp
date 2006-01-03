;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2005 James Earl Prewett

;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


(defpackage :sysloghelp
  (:export #:syslog-date-time-string-to-encoded-time
           #:syslog-date-time-string-to-decoded-time)
  ;(:import-from :cl-ppcre "scan" "scan-to-strings")
)

(in-package :sysloghelp)

(defun syslog-date-time-string-to-encoded-time (string &optional (year (multiple-value-bind (a b c d e year f g h) (get-decoded-time) (declare (ignore a b c d e f g h)) year)))
  (and 
   (multiple-value-bind (matches sub-matches)
       (cl-ppcre::scan-to-strings
        (concatenate
         'string
         "^"
         "(" org.prewett.LoGS::*Month-regexp* ")" " "
         "(" org.prewett.LoGS::*Day-regexp* ")" " "
         "(" org.prewett.LoGS::*Hour-regexp* ")" ":"
         "(" org.prewett.LoGS::*Minute-regexp* ")" ":"
         "(" org.prewett.LoGS::*Second-regexp* ")")
        string)
     (if matches
         (encode-universal-time
          (read-from-string (aref sub-matches 4))
          (read-from-string (aref sub-matches 3))
          (read-from-string (aref sub-matches 2))
          (read-from-string (aref sub-matches 1))
          (let ((monthstring (aref sub-matches 0)))
            (cond ((equal "Jan" monthstring) 1)
                  ((equal "Feb" monthstring) 2)
                  ((equal "Mar" monthstring) 3)
                  ((equal "Apr" monthstring) 4)
                  ((equal "May" monthstring) 5)
                  ((equal "Jun" monthstring) 6)
                  ((equal "Jul" monthstring) 7)
                  ((equal "Aug" monthstring) 8)
                  ((equal "Sep" monthstring) 9)
                  ((equal "Oct" monthstring) 10)
                  ((equal "Nov" monthstring) 11)
                  ((equal "Dec" monthstring) 12)))
          year)))))

(defun syslog-date-time-string-to-decoded-time (string &optional (year (multiple-value-bind (a b c d e year f g h) (get-decoded-time) (declare (ignore a b c d e f g h)) year)))
  (decode-universal-time (syslog-date-time-string-to-encoded-time string year)))
