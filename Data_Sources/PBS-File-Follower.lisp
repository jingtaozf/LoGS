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

(in-package :LoGS)

(defclass PBS-File-Follower (File-Follower)
  ()
  (:documentation "A file-follower that tails PBS log files and understands how to look for the next file in the sequence"))

;; i /think/ this works
;; is there ever a time when tomorrow is more than 24 hours away?
(defun increment-filename (filename)
  (multiple-value-bind (matches sub-matches)
      (cl-ppcre::scan-to-strings "(....)(..)(..)" filename)
    (when matches
      (let* ((year (read-from-string (aref sub-matches 2)))
             (month (read-from-string (aref sub-matches 1)))
             (date (read-from-string (aref sub-matches 0)))
             ;; we assume tomorrow's date is the date that happens to be
             ;; at 24 hours after noon of the previous day
             ;; (that way leap seconds don't kill us)  Thanks #lisp!
             (newtime (+ (* 24 60 60)
                        (encode-universal-time
                         0 0 12
                         year
                         month
                         date
                         ))))
        (multiple-value-bind (x xx xxx date month year)
            (decode-universal-time newtime)
          (declare (ignore x xx xxx))
          (format () "~D~2,'0D~2,'0D" year month date))))))

(defmethod get-line ((ff pbs-file-follower))
"Return the next line of this file.  We refuse to read eof.  When we 
have reached end of the file, we check to see if there is a new inode 
associated with our filename. if there is, we start following that filename."
  (progn
    (when (not (filestream ff))
      (start-file-follower ff))
    (if (peek-char nil (filestream ff) ())
        (read-line (filestream ff) nil)
        (let* ((next-filename (increment-filename (filename ff)))
               (next-inode (get-inode-from-filename next-filename)))
          (when next-inode
            (setf (filename ff) next-filename)
            (read-line (start-file-follower ff) ()))))))
