; Logs extensible (common-lisp based) log/event analysis engine/language
; Copyright (C) 2003-2005 James Earl Prewett

; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(defpackage :ApacheHelp
  (:export "ADD-DAYHIT" "DUMP-ALL-GNUPLOT" "DUMP-MONTH-YEAR-GNUPLOT" "ADD-WORM-HIT" "WORM-COUNT")
)

(in-package :ApacheHelp)

(let ((worm-tracker (make-hash-table :test #'equal)))

  (defun add-worm-hit (ip worm time)
    (multiple-value-bind (hval exists) (gethash ip worm-tracker)
                         (if exists
                             (if (assoc worm hval)
                                 (if (not (member time (cadr (assoc worm hval))))
                                     (rplacd (assoc worm hval) (list (cons time (cadr (assoc worm hval))))))
                               (setf (gethash ip worm-tracker) (cons (list worm (list time)) (gethash ip worm-tracker))))
                           (setf (gethash ip worm-tracker) (list (list worm (list time)))))))
  
  (defun worm-count (worm)
    (maphash #'(lambda (k v)
                 (format t "~A => ~A (~A)~%" k v (assoc worm v))) worm-tracker))
                         
  )

;; Closures to assist with plotting
(let ((date-hash (make-hash-table :test #'equal)))
  
  ; add a hit on a given day (in dd/month/yyyy where month is 3-letter abbrev)
  (defun add-dayhit (inday)
    (let ((day (apache2num-date inday)))
      (if (gethash day date-hash)
          (multiple-value-bind (val exists) (gethash day date-hash)
            (setf (gethash day date-hash) (+ val 1)))
        (setf (gethash day date-hash) 1))))
  
  ; simple dumper
  (defun dump-dayhits-gnuplot-style (data ticfunc)
    (labels ((setxtics (ls cval str)
                       (cond ((not (or (null ls) (null (cdr ls))))
                              (setxtics (cdr ls) (+ cval 1)
                                        (concatenate 'string str (format nil "\"~A\" ~A, " (if (funcall ticfunc (caar ls) cval)
                                                                                               (caar ls)
                                                                                             "") cval))))
                               
                             (t (concatenate 'string str (format nil "\"~A\" ~A )" (if (funcall ticfunc (caar ls) cval)
                                                                                       (caar ls)
                                                                                     "")
                                                                 cval)))))
             
             
             (dump-data (ls cval str)
                        (if ls (dump-data (cdr ls) (+ cval 1)
                                          (concatenate 'string str (format nil "~T~A~T~A~%" cval (cadar ls))))
                          str))
               )
            (progn
              ;; set xtics to be dates
              (format t "~A~%" (setxtics data 0 "set xtics ( "))
              ;; sort the hashtable's output by date (custom sorter?)
              (format t "~%")
              (format t "plot \"-\" using 1:2 w impulses~%")
              (format t "~A" (dump-data data 0 ""))
              (format t "end~%pause -1~%")
              )
            )
    )    

  (defun dump-sort-dayhits ()
    (let ((ls '()))
      (progn
        (maphash #'(lambda (k v)
                     (push (list k v) ls)) date-hash)
        (sort ls #'(lambda (a b)
                     (not (equal a b)))))))

  (defun dump-all-gnuplot ()
      (dump-dayhits-gnuplot-style (dump-sort-dayhits) 
                                  #'(lambda (date cval)
                                      (destructuring-bind (yearstr monthstr daystr)
                                          (cl-ppcre:split "-" date)
                                        (let ((day (read-int daystr)))
                                          (if (= day 1)
                                              day
                                            nil))))))
  
  (defun dump-month-year-gnuplot (month year)
    (if (and (numberp month) (> month 0) (< month 13)
             (numberp year) (> year 1000) (< year 10000))

        (let ((ls '()))
          (progn
            (maphash #'(lambda (k v)
                         (destructuring-bind (kyear kmonth kday) (cl-ppcre:split "-" k)
                                             (if (and (equal (format nil "~A" year) kyear)
                                                      (equal (format nil "~A" month) kmonth))
                                                 (push (list kday v) ls)))) date-hash)

            (dump-dayhits-gnuplot-style 
             (sort ls #'(lambda (a b) (not (equal a b))))
             #'(lambda (date cval)
                 (if (= 0 (mod cval 5))
                     t
                   nil))
                 )))))
  )

(defun read-int (str)
  (if (every #'digit-char-p str)
      (let ((acc 0))
        (dotimes (pos (length str))
          (setf acc (+ (* acc 10)
                        (digit-char-p (char str pos)))))
        acc)
    nil))

(defun apache2num-date (apachedate)
  (destructuring-bind (day month year)
                      (cl-ppcre:split "/" (car (cl-ppcre:split ":" apachedate :limit 2)))
                      (concatenate 'string year "-" 
                                   (cdr (assoc (string-downcase month) '(("jan" . "1")
                                                                         ("feb" . "2")
                                                                         ("mar" . "3")
                                                                         ("apr" . "4")
                                                                         ("may" . "5")
                                                                         ("jun" . "6")
                                                                         ("jul" . "7")
                                                                         ("aug" . "8")
                                                                         ("sep" . "9")
                                                                         ("oct" . "10")
                                                                         ("nov" . "11")
                                                                         ("dec" . "12")) :test #'equal))
                                   "-" day)))
