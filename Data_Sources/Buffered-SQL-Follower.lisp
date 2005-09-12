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



(when +use-sql+

  #-asdf
  (error "ASDF package is not loaded!... bailing!~%")

  (use-package :asdf)

  (push "/Users/dl/project-fw/clsql-3.2.1/" asdf:*central-registry*)
  (push "/Users/dl/project-fw/uffi-1.5.1/" asdf:*central-registry*)
  (asdf:operate 'asdf:load-op :clsql)

  (use-package :clsql)

  (defclass buffered-sql-Follower (Data-Source)
    ((message-list :accessor message-list 
                   :initarg :message-list
                   :initform ())
     (buffer-size :accessor buffer-size
                  :initarg :buffer-size
                  :initform 1024)
     (current-row :accessor current-row
                  :initarg :current-row
                  :initform 1)
     (username :accessor username
               :initarg :username)
     (password :accessor password
               :initarg :password)
     (host :accessor host
           :initarg :host)
     (database :accessor database
               :initarg :database)
     (thequery :accessor thequery
               :initarg :thequery)))
  
  (defmethod initialize-instance :after ((buffered-sql-follower buffered-sql-follower) &rest rest)
    (when (not *default-database*)
      (connect `( 
                        ,(host buffered-sql-follower)
                        ,(database buffered-sql-follower)
                        ,(username buffered-sql-follower)
                        ,(password buffered-sql-follower)))))

  (defmethod get-next-chunk ((buffered-sql-follower buffered-sql-follower))
    (progn
      (setf (message-list buffered-sql-follower)
            (query (format () "~A where id >= ~A and id < ~A" 
                                  (thequery buffered-sql-follower)
                                  (current-row buffered-sql-follower)
                                  (+ (current-row buffered-sql-follower) 
                                     (buffer-size buffered-sql-follower)))))
      (setf (current-row buffered-sql-follower) 
            (+ (current-row buffered-sql-follower) (buffer-size buffered-sql-follower)))))

  (defmethod print-thequery ((buffered-sql-follower buffered-sql-follower))
    (format () "~A where id >= ~A and id < ~A" 
            (thequery buffered-sql-follower)
            (current-row buffered-sql-follower)
            (+ (current-row buffered-sql-follower) 
               (buffer-size buffered-sql-follower))))

  (defmethod get-line ((Buffered-Sql-Follower Buffered-Sql-Follower))
    (progn
      (when
          (not (message-list buffered-sql-follower))
        (get-next-chunk buffered-sql-follower))
    
      (let ((first-message (car (message-list Buffered-Sql-Follower))))
        (setf (message-list Buffered-Sql-Follower)
              (cdr (message-list Buffered-Sql-Follower)))
        first-message)))
       

  (defmethod get-logline ((Buffered-Sql-Follower Buffered-Sql-Follower))
    (let ((line (get-line Buffered-Sql-Follower)))
      (when line
        (make-instance 'message :message line))))
  )