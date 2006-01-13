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

(in-package :org.prewett.LoGS)

;;;; The doubly-linked-list-item class

(defclass doubly-linked-list-item ()
  ((data :initform ()
         :accessor data
         :initarg :data)
   (rlink :initform ()
          :accessor rlink)
   (llink :initform ()
          :accessor llink)))

;;;; The Doubly-linked-list class

(defclass doubly-linked-list ()
  ((head :initform ()
         :accessor head)
   (tail :initform ()
         :accessor tail)
   (list-entries :initform (make-hash-table :test #'equal)
                 :accessor list-entries)
  ))


(defgeneric dll-insert (doubly-linked-list neighbor-item insert-item &key direction))

;; general case
(defmethod dll-insert ((doubly-linked-list doubly-linked-list)
                       (neighbor-item doubly-linked-list-item)
                       (insert-item doubly-linked-list-item) 
                       &key direction)
  (if (gethash 
       (data insert-item)
       (list-entries doubly-linked-list))
      (format t "item already exists in list~%")
    
      (cond
        ((equal direction :before)
         (progn
           (LoGS-debug "inserting ~A into ~A before ~A~%" insert-item doubly-linked-list neighbor-item)
           (when (llink neighbor-item)
             (setf (rlink (llink neighbor-item)) insert-item))
           (setf (rlink insert-item) neighbor-item)
           (setf (llink insert-item) (llink neighbor-item))
           (setf (llink neighbor-item) insert-item)
           ;; if the neighbor was the head, we are now the head
           (when (eq (head doubly-linked-list) neighbor-item)
             (setf (head doubly-linked-list) insert-item))))
        ((equal direction :after)
         (progn
           (LoGS-debug "inserting ~A into ~A after ~A~%" 
                       insert-item doubly-linked-list neighbor-item)
           (when (rlink neighbor-item)
             (setf (llink (rlink neighbor-item)) insert-item))
           (setf (llink insert-item) neighbor-item)
           (setf (rlink insert-item) (rlink neighbor-item))
           (setf (rlink neighbor-item) insert-item)
           ;; if the neighbor was the tail, we are now the tail
           (when (eq (tail doubly-linked-list) neighbor-item)
             (setf (tail doubly-linked-list) insert-item))
           ))
        (t
         (error "unknown direction: ~A~%" direction)))))
  
;; unencapsulated case
(defmethod dll-insert ((doubly-linked-list doubly-linked-list)
                       (neighbor-item doubly-linked-list-item)
                       insert-item
                       &key direction)
  (unless (gethash 
           insert-item
           (list-entries doubly-linked-list))
    (dll-insert doubly-linked-list
                neighbor-item
                (make-instance 'doubly-linked-list-item :data insert-item)
                :direction direction)))
  
;; null neighbor
(defmethod dll-insert ((doubly-linked-list doubly-linked-list)
                       (neighbor-item (eql NIL))
                       (insert-item doubly-linked-list-item)
                       &key direction)
  (unless (gethash 
           (data insert-item) 
           (list-entries doubly-linked-list))
    (cond ((equal direction :before)
           (progn
             (LoGS-debug "inserting item ~A into ~A before nil~%" 
                         insert-item doubly-linked-list)
             (if (head doubly-linked-list)
                 (dll-insert doubly-linked-list (head doubly-linked-list) insert-item :direction direction)
                 (progn
                   (setf (head doubly-linked-list) insert-item)
                   (setf (tail doubly-linked-list) insert-item)))))
          ((equal direction :after)
           (progn
             (LoGS-debug "inserting item ~A into ~A after nil~%" insert-item doubly-linked-list)
             (if (tail doubly-linked-list)
                 (dll-insert doubly-linked-list (tail doubly-linked-list) insert-item :direction direction)
                 (progn
                   (setf (head doubly-linked-list) insert-item)
                   (setf (tail doubly-linked-list) insert-item))))))))
        
                

(defmethod dll-insert ((doubly-linked-list doubly-linked-list)
                       (neighbor-item (eql NIL))
                       insert-item
                       &key direction)
  (unless (gethash insert-item
           (list-entries doubly-linked-list))
    (dll-insert doubly-linked-list
                neighbor-item
                (make-instance 'doubly-linked-list-item :data insert-item)
                :direction direction)))

(defmethod dll-insert :after ((doubly-linked-list doubly-linked-list)
                              neighbor-item
                              (insert-item doubly-linked-list-item)
                              &key direction)
  (declare (ignore direction neighbor-item))
  (setf (gethash 
         (data insert-item) 
         (list-entries doubly-linked-list)) 
        insert-item))

 (defgeneric enqueue (data-structure item)
   (:documentation "add the given item to the data structure (queue)"))

(defmethod enqueue ((doubly-linked-list doubly-linked-list)
                    insert-item)
  (dll-insert doubly-linked-list (tail doubly-linked-list) insert-item 
              :direction :after))

(defgeneric dll-delete (doubly-linked-list item-to-delete)
  (:documentation "remove the given item from the data structure (queue)"))

(defmethod dll-delete ((doubly-linked-list doubly-linked-list)
                       item-to-delete)
  (let ((lookup (gethash item-to-delete (list-entries doubly-linked-list))))
    (when lookup
      (dll-delete doubly-linked-list lookup))))

(defgeneric remove-item (remove-item)
  (:documentation "unlink a dlli from its neighbors; cut it  out of the list"))

(defmethod remove-item ((remove-item doubly-linked-list-item))
  (with-slots ((before llink)(after rlink)) remove-item
    (when before
      (setf (rlink before) after))
    (when after
      (setf (llink after) before))))

(defmethod dll-delete ((doubly-linked-list doubly-linked-list)
                       (item-to-delete doubly-linked-list-item))
  (progn
    (remove-item item-to-delete)
    (when (eql (head doubly-linked-list) item-to-delete) ; head
      (setf (head doubly-linked-list)
            (if (eql item-to-delete (rlink item-to-delete))
                ()
                (rlink item-to-delete))))
    (when (eql (tail doubly-linked-list) item-to-delete) ;tail
      (setf (tail doubly-linked-list)
            (if (eql item-to-delete (llink item-to-delete))
                ()
                (llink item-to-delete))))))

(defmethod dll-delete :after ((doubly-linked-list doubly-linked-list)
                              (item-to-delete doubly-linked-list-item))
  (remhash (data item-to-delete) (list-entries doubly-linked-list)))
