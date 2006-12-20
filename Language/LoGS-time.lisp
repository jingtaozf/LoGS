;;;; LoGS extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2006 James Earl Prewett

;;;; This file is a part of LoGS.  LoGS is free software; you can
;;;; redistribute it and/or modify it under the terms of the GNU
;;;; General Public License as published by the Free Software
;;;; Foundation; either version 2 of the License, or (at your option)
;;;; any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
(in-package :language)

(defun recognize-hhmmss (str tokens)
  "Recognize HHMMSS separated by ':'s.  All characters in the string
must be digits or :s, & the string must be of length 8."
  (declare (type string str) (ignore tokens))
  (and (eql (length str) 8)
       (string-equal ":" (subseq str 2 3))
       (string-equal ":" (subseq str 5 6))
       (every #'digit-char-p (subseq str 0 2))
       (every #'digit-char-p (subseq str 3 5))
       (every #'digit-char-p (subseq str 6 8))
       (cybertiggyr-time::make-broken-time 
        :ss (parse-integer str :start 6 :end 8) 
        :mm (parse-integer str :start 3 :end 5) 
        :hh (parse-integer str :start 0 :end 2)
        :dd (funcall *default-day*)
        :mo (funcall *default-month*)
        :yr (funcall *default-year*)
        :zone nil)))

(defun recognize-hhmmss-tomorrow (str tokens)
  "Recognize HHMMSS separated by ':'s.  All characters in the string
must be digits or :s, & the string must be of length 17."
  (declare (type string str) (ignore tokens))
  (and (eql (length str) 17)
       (string-equal "tomorrow" (subseq str 9 17))
       (string-equal ":" (subseq str 2 3))
       (string-equal ":" (subseq str 5 6))
       (every #'digit-char-p (subseq str 0 2))
       (every #'digit-char-p (subseq str 3 5))
       (every #'digit-char-p (subseq str 6 8))
       (let ((tomorrow-date (+ 1 (funcall *default-day*))))
         (cybertiggyr-time::make-broken-time 
          :ss (parse-integer str :start 6 :end 8) 
          :mm (parse-integer str :start 3 :end 5) 
          :hh (parse-integer str :start 0 :end 2)
          :dd (if (< tomorrow-date 31)
                  tomorrow-date
                  1)
          :mo (if (> 31 (+ 1 (funcall *default-day*)))
                  (funcall *default-month*)
                  (+ 1 (funcall *default-month*)))
          :yr (funcall *default-year*)
          :zone nil))))
