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

;(in-package "LISP")
;(export '(probe-file file-stat  FILE-STAT-INO))

(defclass File-Follower (Data-Source)
  (
   (Filename   :accessor Filename :initarg :Filename)
   (FileStream :accessor FileStream :initform ())
   (Inode      :accessor Inode :initform ())
   (offset     :accessor offset :initform ())
   )
  )

(defmethod start-file-follower ((ff file-follower))
  (and
   (probe-file (Filename ff))
   ;; get the inode of the file we are reading
   (multiple-value-bind (ok dev ino)
       (unix:unix-stat (Filename ff))
       (declare (ignore ok dev))
     (setf (Inode ff) ino))
   (setf (Filestream ff) (open (Filename ff) :direction :input))))


;; does inode rollover work?
(defmethod get-line ((ff file-follower))
  "Return the next line of this file.  We refuse to read eof.  When we 
have reached end of the file, we check to see if there is a new inode 
associated with our filename. if there is, we start following that filename."
  (or
   (and 
                                        ;we're at the end of the file
    (not (eq (file-length (FileStream ff))
       (file-position (FileStream ff))))
   (read-line (filestream ff) ()))
   ;; eof, possibly with new file
   (or 
    ; there is a new file to look at
    (and 
     (probe-file (Filename ff))
     (let ((stat-inode (multiple-value-bind (ok dev ino)
                           (unix:unix-stat (filename ff))
                         (declare (ignore ok dev))
                         ino)))
       (if
        (eq (Inode ff) stat-inode)
        ()
        (progn
          (format t "starting new file follower~%")
          (read-line (start-file-follower ff) ()))))))))


(defun psuedo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof))
      (format t "~A~%" line))))
