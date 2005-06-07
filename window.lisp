
; Logs extensible (common-lisp based) log/event analysis engine/language
; Copyright (C) 2003-2005 James Earl Prewett

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

(in-package :LoGS)

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
  (let ((current-item (head (data window)))
        (mintime (- *now* (* (window-length window) 
                             INTERNAL-TIME-UNITS-PER-SECOND))))
    (loop 
         while current-item
         do
         (if (< (car (data current-item)) mintime)
             (remove-window-entry window current-item)
             (return t))
         (setf current-item (rlink current-item)))))

(defmethod check-limits :around ((window window))
  (progn
    (remove-old window)
    (call-next-method)))

(defmethod add-item :before ((window window) item &rest rest)
  (remove-old window))