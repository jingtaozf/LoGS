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

(in-package :LoGS)

(defclass timeout-object ()
  ((timeout :initarg :timeout
            :accessor timeout
            :initform ()
            :type (or null integer))))

(defmethod sort-timeouts ((x timeout-object) (y timeout-object))
  (let ((t-x (timeout x))
        (t-y (timeout y)))
    (declare (integer t-x))
    (declare (integer t-y))
    (> t-x t-y)))

;; a priority queue to hold things that can time out
(defvar *timeout-object-timeout-queue*
  (make-instance 'priority-queue 
                 :comparison-function 
                 #'sort-timeouts)
  "A priority queue to hold things that can time out.")


(defmethod initialize-instance :after ((timeout-object timeout-object)
                                       &rest rest)
  (declare (ignore rest))
  (when (timeout timeout-object)
    (enqueue *timeout-object-timeout-queue*
             timeout-object)))


(defmethod (setf timeout) :after (new-value (timeout-object timeout-object))
  (progn
    (dll-delete *timeout-object-timeout-queue* timeout-object)
    (enqueue *timeout-object-timeout-queue* timeout-object)))

(defmethod exceeded-timeout-p ((timeout-object timeout-object) time)
  (declare (integer time))
  (with-slots (timeout) timeout-object
    (when timeout 
      (> time timeout))))

(defmethod check-limits OR ((timeout-object timeout-object)) 
           (let ((ret (exceeded-timeout-p timeout-object *now*)))
             (LoGS-debug "checking timeout object limit.  ret: ~A~%" ret)
             ret))
