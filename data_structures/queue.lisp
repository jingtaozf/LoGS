;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2007 James Earl Prewett

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

(defclass queue ()
  ((head :initform ()
	 :initarg :head
	 :accessor head)
   (tail :initform ()
	 :initarg :tail
	 :accessor tail)
   (data :accessor data
	 :initform ()))
  (:documentation "The queue class implements a queue"))

(defgeneric empty-p (data-structure)
  (:documentation "return non-nil if the data structure is empty."))

(defmethod empty-p ((q queue))
  (with-slots
        (head) q
    (not head)))

(defgeneric enqueue (queue item)
  (:documentation "add an item to some sort of queue."))

(defmethod enqueue ((q queue) item)
  (progn
    (setf (tail q) item) ;; this is the last item, it is the tail
    (or (head q) (setf (head q) item)) ;; there is no head, we must be it
    (setf (data q) 
	  (append (data q) (list item)))))

(defgeneric dequeue (queue item)
  (:documentation "remove an item from some sort of queue."))

(defmethod dequeue ((q queue) item)
  (let ((queue-data (setf (data q)
			  (remove item (data q)))))
    (or queue-data
	(setf (head q) ())
	(setf (tail q) ()))
    (and (eq item (tail q))
	(setf (tail q) (car (last (data q)))))
    (and (eq item (head q))
	 (setf (head q) 
	       (first (data q))))
  queue-data))


