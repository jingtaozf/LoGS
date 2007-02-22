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
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

(in-package :org.prewett.LoGS)

;; it seems that this should be a doubly-linked-list so that we could more 
;; easily remove items!
(defclass window (context)
  ((data :initform (make-instance 'doubly-linked-list)
         :accessor data)
   (window-length :initarg :window-length
                  :accessor window-length
                  :initform 0)))

(defmethod add-item ((window window) item &rest rest)
  (declare (ignore rest))
  (progn
    (incf (ecount window))
    (dll-insert (data window) (tail (data window)) (list *now* item) :direction :after)))

(defmethod remove-window-entry ((window window) item)
  (progn
    (decf (Ecount window))
    (dll-delete (data window)
                item)))

(defmethod remove-old ((window window))
  (loop 
     for current-item = (head (data window)) then (rlink current-item)
     with mintime = (- *now* (* (window-length window) 
                                  INTERNAL-TIME-UNITS-PER-SECOND))
     while current-item
     do
       (if (< (car (data current-item)) mintime)
           (remove-window-entry window current-item)
           (return t))))

(defmethod check-limits :around ((window window))
  (progn
    (remove-old window)
    (call-next-method)))

(defmethod add-item :before ((window window) item &rest rest)
  (declare (ignore rest))
  (remove-old window))

(defmethod write-context ((window window) stream)
  (loop for i from 0 below (ecount window) 
     for current = (head (data window)) then (rlink current)
     while current
     do
       (format stream "~A~%" 
               (message (cadr (data current))))))

(defmacro ensure-window (&rest rest &key name &allow-other-keys)
  (let ((named-context (gensym)))
  `(let ((,named-context (get-context ,name)))
     (when (and ,name ,named-context)
       (LoGS-debug "a context named ~A already exists at~%" ,name ,named-context))
     (or
      (when ,name
        ,named-context)
      (make-instance 'window
                     ,@rest)))))
