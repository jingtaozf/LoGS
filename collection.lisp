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

;;; collections
;; a class that holds things. 
;; When the underlying array runs out of space, it is dynamically
;; resized.
(defclass collection (named-object) 
  ((Ecount :initform 0 :accessor Ecount 
           :documentation "the current number of items in the collection")
   (Emax   :initform 0 :accessor Emax :initarg :Emax 
           :documentation "the maximum number of entries before a resize of the collection.")
   (curr-pow :initform 0 :accessor curr-pow  
             :documentation "book keeping; what power of 2 is the number of elements?  Increment when we resize.")
   (data :initform (make-array '(0) :adjustable t) :accessor data 
         :documentation "the array of entries in the collection"))
  (:documentation "A fancy array class that dynamically grows as needed"))

(defgeneric add-item (collection item &rest rest)
  (:documentation "add an item to a collection."))

;; add an item to a collection.  resize if necessary.
(defmethod add-item ((collection collection) (item t) &rest rest)
  "add an item to a collection.  The collecton will dynamically re-size
if it needs to."
  (declare (ignore rest)) 
  (let ((Ecount (Ecount collection))
        (Emax (Emax collection)))
    (when
        (>= Ecount Emax)
      (setf (data collection)
            (adjust-array 
             (data collection) 
             (setf (Emax collection)
                   (expt 2 
                         (incf (curr-pow collection)))))))
    (setf (aref (data collection) Ecount) item)
    (incf (Ecount collection))))
