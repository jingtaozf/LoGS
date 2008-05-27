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

(defclass List-Follower (Data-Source)
  ((message-list :accessor message-list 
                 :initarg :message-list
                 :initform ())))


(defmethod get-line ((List-Follower List-Follower))
  (let ((first-message (car (message-list List-Follower))))
    (setf (message-list List-Follower)
          (cdr (message-list List-Follower)))
    first-message))

(defmethod get-logline ((List-Follower List-Follower))
  (let ((line (get-line List-Follower)))
    (when line
      (make-instance 'message :message line))))
