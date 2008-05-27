;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2008 James Earl Prewett

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

(in-package :org.prewett.LoGS)

(defclass PBS-File-Follower (File-Follower)
  ()
  (:documentation "A file-follower that tails PBS log files and understands how to look for the next file in the sequence"))

(defun remove-last (list)
  (when (> (length list) 1)
    (cons (car list)
          (remove-last (cdr list)))))

(defun increment-filename (fname)
  "Given a filename such as 20050325, return the next day's filename, 20050326."
  ;; only increment the filename part of the pathname
  (let* ((pathparts (cl-ppcre::split "/" fname))
         (filename (car (last pathparts))))
    (format
     () "~{~A/~}~A" (remove-last pathparts)
     (multiple-value-bind (matches sub-matches)
         (cl-ppcre::scan-to-strings "(....)(..)(..)" filename)
       (when matches
         (let* ((year (read-from-string (aref sub-matches 0)))
                (month (read-from-string (aref sub-matches 1)))
                (date (read-from-string (aref sub-matches 2)))
                ;; we assume tomorrow's date is the date that happens to be
                ;; at 24 hours after one second before midnight of the previous 
                ;; day 
                ;; 
                ;; we have a bug whenever there is a leap day (or more); I'm not
                ;; worried.
                (newtime (+ (* 24 60 60)
                            (encode-universal-time
                             59 59 23
                             date
                             month
                            year
                            ))))
           (multiple-value-bind (x xx xxx date month year)
               (decode-universal-time newtime)
             (declare (ignore x xx xxx))
             (let ((ret (format () "~D~2,'0D~2,'0D" year month date)))
               (LoGS-debug "incrementing filename: ~A to filename: ~A~%"
                         filename ret)
               ret))))))))

(defmethod get-line ((ff pbs-file-follower))
"Return the next line of this file.  We refuse to read eof.  When we 
have reached end of the file, we check to see if there is a new inode 
associated with our filename. if there is, we start following that filename."
  (progn
    (when (not (filestream ff))
      (LoGS-debug "starting file follower~%")
      (start-file-follower ff))
    (if (peek-char nil (filestream ff) ())
        (read-line (filestream ff) nil)
        (let* ((next-filename (increment-filename (filename ff)))
               (next-inode (get-inode-from-filename next-filename)))
          (when next-inode
            (LoGS-debug "opening next filename: ~A~%" next-filename)
            (setf (filename ff) next-filename)
            (read-line (start-file-follower ff) ()))))))
