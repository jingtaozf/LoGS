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

;; a priority queue to hold things that can time out
(defvar *timeout-object-timeout-queue*
  (make-instance 'priority-queue 
                 :comparison-function 
                 (lambda (x y)
                   (let ((t-x (timeout x))
                         (t-y (timeout y)))
                     (declare (fixnum t-x t-y))
                     (> t-x t-y))))
  "A priority queue to hold things that can time out.")

(defclass timeout-object ()
  ((timeout :initarg :timeout
            :accessor timeout
            :initform ())))


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
  (with-slots (timeout) timeout-object
    (when timeout 
      (> time timeout))))

(defmethod check-limits ((timeout-object timeout-object))
  (exceeded-timeout-p timeout-object *now*))