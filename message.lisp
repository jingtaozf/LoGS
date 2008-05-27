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

(declaim (OPTIMIZE (SPEED 3) (debug 0) (SAFETY 3)))

;;; a message class.
(defclass message () 
  ((message :initarg :message :accessor message :initform ())
   (from-file :initarg :from-file :accessor from-file :initform ())
   (tag :initarg :tag :accessor tag :initform ()))
  (:documentation "A class to hold messages.  You probably want string-message instead."))

;;; a string message class

(defclass string-message (message) ()
  (:documentation "A class that holds string messages."))

(defmethod print-object ((obj message) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (message) obj
      (format stream "\"~A\"" message))))

(defun message-reader (stream char arg)
  (declare (ignore char arg))
  (let ((start-char (read-char stream)))
    (unless (or
             (equal #\' start-char)
             (equal #\" start-char))
      (error "expecting message string"))
    (make-instance 
     'message
     :message
     (with-output-to-string (output)
      (loop as char = (read-char stream)
           until (equal char start-char)
           do
           (format output "~C" char))))))

(set-dispatch-macro-character #\# #\m #'message-reader)
