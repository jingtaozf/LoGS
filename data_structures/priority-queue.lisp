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

(defclass priority-queue-item (doubly-linked-list-item)
  ())

(defclass priority-queue (doubly-linked-list)
  ((comparison-function :initform (lambda (x y) 
                                    (declare (fixnum x y))
                                    (> x y))
                        :initarg :comparison-function 
                        :accessor comparison-function)
   (elements :initform (make-hash-table :test #'equal) :accessor elements)))

;; add an item to the priority queue

;; (defmethod enqueue ((pq priority-queue) item)
;;     (LoGS-debug "inserting item: ~A into priority queue ~A~%" item pq)
;;     ;; find the right place to insert the item
;;     (loop with x
;;        initially (setf x (head pq))
;;        do
;;          (cond ((or (not x)
;;                     (not
;;                      (funcall (comparison-function pq) item (data x))))
;;                 (progn
;;                   (LoGS-debug "inserting before item: ~A~%" (when x (data x)))
;;                   (return (dll-insert pq x item :direction :before))))
;;                ((eq x (tail pq))
;;                 (progn
;;                   (LoGS-debug "inserting after item: ~A~%" (when x (data x)))
;;                   (return (dll-insert pq x item :direction :after))))
;;                (t (setf x (rlink x))))))

(defmethod enqueue ((pq priority-queue) &rest items)
  (loop for item in items
        do
        (LoGS-debug "inserting item: ~A into priority queue ~A~%" item pq)
        ;; find the right place to insert the item
        (loop with x
              initially (setf x (head pq))
              do
              (cond ((or (not x)
                         (not
                          (funcall (comparison-function pq) item (data x))))
                     (progn
                       (LoGS-debug "inserting before item: ~A~%" (when x (data x)))
                       (return (dll-insert pq x item :direction :before))))
                    ((eq x (tail pq))
                     (progn
                       (LoGS-debug "inserting after item: ~A~%" (when x (data x)))
                       (return (dll-insert pq x item :direction :after))))
                    (t (setf x (rlink x)))))))

(defmethod check-limits OR ((pq priority-queue))
  (declare (OPTIMIZE (SPEED 0) (DEBUG 3) (SAFETY 3)))  
  (loop for dlli = (head pq) then (rlink dlli)
     when t
     do (LoGS-debug "checking pq: ~A dlli: ~A~%" pq dlli)
     when (not dlli)
     do
       (return)
     else when (check-limits (data dlli))
     do 
       (logs-debug "removing ~A (~A) from priority queue~%" (data dlli) (dead-p (data dlli)))
       (dll-delete pq dlli)
       (kill (data dlli))
       (return)
       ))
