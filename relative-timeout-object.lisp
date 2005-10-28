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

(defclass relative-timeout-object ()
  ((relative-timeout :initarg :relative-timeout
                     :accessor relative-timeout
                     :initform ()
                     :type (or null integer))
   (next-timeout :accessor next-timeout
                 :initform ()
                 :type (or null integer))))

(defmethod update-relative-timeout ((relative-timeout-object relative-timeout-object))
  (with-slots (relative-timeout) relative-timeout-object
    (LoGS-debug "updating relative timeout for object: ~A~%" relative-timeout-object)
    (when relative-timeout
      (setf (next-timeout relative-timeout-object)
            (+ *now* (* INTERNAL-TIME-UNITS-PER-SECOND 
                        relative-timeout))))))

(defmethod sort-relative-timeouts ((x relative-timeout-object) (y relative-timeout-object))
  (let ((t-x (next-timeout x))
        (t-y (next-timeout y)))
    (declare (integer t-x))
    (declare (integer t-y))
    (> t-x t-y)))

;; a priority queue to hold things that can time out
(defvar *relative-timeout-object-timeout-queue*
  (make-instance 'priority-queue 
                 :comparison-function 
                 #'sort-relative-timeouts)
  "A priority queue to hold things that can time out.")


(defmethod initialize-instance :after ((relative-timeout-object relative-timeout-object)
                                       &rest rest)
  (declare (ignore rest))
  (when (relative-timeout relative-timeout-object)
    (setf (next-timeout relative-timeout-object)
          (+ *now* (* INTERNAL-TIME-UNITS-PER-SECOND 
                      (relative-timeout relative-timeout-object))))
    (LoGS-debug "adding object: ~A to relative queue~%"
                          relative-timeout-object)
    (enqueue *relative-timeout-object-timeout-queue*
             relative-timeout-object)))


(defmethod (setf next-timeout) :after (new-value (relative-timeout-object relative-timeout-object))
  (progn
    (LoGS-debug "removing object: ~A from relative queue~%"
                          relative-timeout-object)
    (dll-delete *relative-timeout-object-timeout-queue* relative-timeout-object)
    (LoGS-debug "adding object: ~A to relative queue~%" relative-timeout-object)
    (enqueue *relative-timeout-object-timeout-queue* relative-timeout-object)))

(defmethod exceeded-relative-timeout-p ((relative-timeout-object relative-timeout-object) time)
  (declare (integer time))
  (with-slots (next-timeout) relative-timeout-object
    (when next-timeout 
      (> time next-timeout))))

(defmethod check-limits OR ((relative-timeout-object relative-timeout-object))
           (let ((ret (exceeded-relative-timeout-p relative-timeout-object *now*)))
             (LoGS-debug "checking relative-timeout-object limits. ret: ~A~%" ret)
             ret))

