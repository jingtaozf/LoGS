; Logs extensible (common-lisp based) log/event analysis engine/language
; Copyright (C) 2003-2004 James Earl Prewett

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


(defclass File-Follower ()
  ((Filename   :accessor Filename :initarg :Filename)
   (FileStream :accessor FileStream :initform ())
   (Inode      :accessor Inode :initform ())
   (offset     :accessor offset :initform ())))

(defmethod start-file-follower ((ff file-follower))
  "Associate the file follower with the file it is supposed to be following."
  (and
   (probe-file (Filename ff))
   ;; get the inode of the file we are reading
   (let ((ino (get-inode-from-filename (filename ff))))
     (and ino
          (setf (Inode ff) ino)
          (setf (Filestream ff) (open (Filename ff) :direction :input))))))

(defun get-inode-from-filename (Filename)
  "Given a filename, return the inode associated with that filename."
  #+allegro
  (nth-value 1 
             (excl::filesys-inode filename))
  #+cmu
  (nth-value 2
             (unix:unix-stat filename))
  #+sbcl
  (nth-value 2 
             (SB-UNIX:UNIX-STAT filename))
  #+openmcl
  (nth-value 4
             (CCL::%STAT filename))
  #+lispworks
  (system::file-stat-inode (system::get-file-stat filename)))

(defun get-file-length-from-filename (filename)
  "Given a filename, return the number of bytes currently in the file."
  (nth-value 8 (unix:unix-stat filename)))

(defmethod set-file-follower-position ((file-follower file-follower)
                                       (position number))
  (file-position
   (filestream file-follower)
   position))

(defmethod set-file-follower-position ((file-follower file-follower)
                                       (position string))
  (cond ((equal position "end")
         (set-file-follower-position
          file-follower
          (get-file-length-from-filename (filename file-follower))))
        ((equal position "start")
         (set-file-follower-position
          file-follower
          0))
        (t (error "unknown position keyword: ~A~%" position))))

;; make sure the new file follower is attached to the file.
(defmethod initialize-instance :after ((ff File-Follower) &rest rest)
  (declare (ignore rest))
  (start-file-follower ff))

(defmethod get-logline ((ff file-follower))
"Wrap the next line of the file associated with the file-follower inside of
a message."
  (let ((line (get-line ff)))
    (when line
         (make-instance 'message :message line))))

;; Thank you Damien Kick!
(defmethod get-line ((ff file-follower))
"Return the next line of this file.  We refuse to read eof.  When we 
have reached end of the file, we check to see if there is a new inode 
associated with our filename. if there is, we start following that filename."
  (if (peek-char nil (filestream ff) nil)
      (read-line (filestream ff) nil)
      (let ((stat-inode (get-inode-from-filename (filename ff))))
        (and (not (eql (inode ff) stat-inode))
             (read-line (start-file-follower ff) nil)))))

