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

;;;; The doubly-linked-list-item class

(defclass doubly-linked-list-item ()
  ((data :initform ()
         :accessor data
         :initarg :data)
   (rlink :initform ()
          :accessor rlink)
   (llink :initform ()
          :accessor llink)))

;; a doubly-linked-list-item should be linked to itself
(defmethod initialize-instance :after 
    ((new-item doubly-linked-list-item) &rest rest)
  (declare (ignore rest))
  (setf (rlink new-item) new-item
        (llink new-item) new-item))

;; insert item between before and after
(defmethod insert-between ((insert-item doubly-linked-list-item)
                             (before doubly-linked-list-item)
                             (after doubly-linked-list-item))
    (setf (rlink insert-item) after
          (llink insert-item) before
          (rlink before) insert-item
          (llink after) insert-item))

(defmethod insert-after ((insert-item doubly-linked-list-item)
                         (before doubly-linked-list-item))
    (insert-between
     insert-item
     before
     (rlink before)))

(defmethod insert-before ((insert-item doubly-linked-list-item)
                          (after doubly-linked-list-item))
  (insert-between
   insert-item
   (llink after)
   after))

(defmethod remove-item ((remove-item doubly-linked-list-item))
  (let ((before (llink remove-item))
        (after (rlink remove-item)))
    (setf (rlink before) after
          (llink after) before)))

;;;; The Doubly-linked-list class

(defclass doubly-linked-list ()
  ((head :initform ()
         :accessor head)
   (tail :initform ()
         :accessor tail)
   (list-entries :initform (make-hash-table :test #'equal)
                 :accessor list-entries)
  ))

;; insert an item at the head of the queue
(defmethod enqueue ((doubly-linked-list doubly-linked-list)
                    insert-item)
  (dll-insert doubly-linked-list (tail doubly-linked-list) insert-item 
              :direction :after))

;; insert an item into the queue
(defgeneric dll-insert (doubly-linked-list neighbor-item insert-item &key direction))

(defmethod dll-insert ((doubly-linked-list doubly-linked-list)
                       neighbor-item insert-item &key direction)
  (declare (ignore direction insert-item neighbor-item doubly-linked-list)))

(defmethod dll-insert ((doubly-linked-list doubly-linked-list)
                       (neighbor-item doubly-linked-list-item)
                       insert-item
                       &key direction)

          (dll-insert doubly-linked-list
                      neighbor-item
                      (make-instance 'doubly-linked-list-item
                                     :data insert-item)
                      :direction direction))

(defmethod dll-insert ((doubly-linked-list doubly-linked-list)
                              (neighbor-item doubly-linked-list-item)
                              (insert-item doubly-linked-list-item)
                              &key direction)
  (progn
    (when (not (tail doubly-linked-list))
      (setf (tail doubly-linked-list) insert-item))
    (when (not (head doubly-linked-list))
      (setf (head doubly-linked-list) insert-item))
    (if (eql direction :after)
        (progn
          (insert-after insert-item neighbor-item)
          (when (or (eql neighbor-item (tail doubly-linked-list))
                    (not (tail doubly-linked-list)))
            (setf (tail doubly-linked-list) insert-item)))
        (progn
          (insert-before insert-item neighbor-item)
          (when (or (eql neighbor-item (head doubly-linked-list))
                    (not (head doubly-linked-list)))
            (setf (head doubly-linked-list) insert-item))))))
    

(defmethod dll-insert :after ((doubly-linked-list doubly-linked-list)
                              (neighbor-item doubly-linked-list-item)
                              (insert-item doubly-linked-list-item)
                              &key direction)
  (declare (ignore direction neighbor-item))
  (setf (gethash 
         (data insert-item) 
         (list-entries doubly-linked-list)) 
        insert-item))

; for insert at beginning or end (or into empty)
(defmethod dll-insert ((doubly-linked-list doubly-linked-list)
                       (neighbor-item list)
                       insert-item 
                       &key direction)
  (let ((enc-insert-item (make-instance 'doubly-linked-list-item
                                  :data insert-item)))
    (setf (gethash insert-item (list-entries doubly-linked-list)) 
          enc-insert-item)
    (dll-insert doubly-linked-list enc-insert-item enc-insert-item 
                :direction direction)))

(defmethod dll-delete ((doubly-linked-list doubly-linked-list)
                       item-to-delete)
  (let ((lookup (gethash item-to-delete (list-entries doubly-linked-list))))
    (when lookup
      (dll-delete doubly-linked-list lookup))))

(defmethod dll-delete ((doubly-linked-list doubly-linked-list)
                       (item-to-delete doubly-linked-list-item))
  (progn
    (remove-item item-to-delete)
    (when (eq (head doubly-linked-list) item-to-delete) ; head
      (setf (head doubly-linked-list)
            (if (eq item-to-delete (rlink item-to-delete))
                ()
                (rlink item-to-delete))))
    (when (eq (tail doubly-linked-list) item-to-delete) ;tail
      (setf (tail doubly-linked-list)
            (if (eq item-to-delete (llink item-to-delete))
                ()
                (llink item-to-delete))))))

(defmethod dll-delete :after ((doubly-linked-list doubly-linked-list)
                              (item-to-delete doubly-linked-list-item))
  (remhash (data item-to-delete) (list-entries doubly-linked-list)))
