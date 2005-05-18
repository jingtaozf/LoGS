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

(defclass killable-item () 
  ((dead :initform ()
         :accessor dead-p
         :initarg :dead))
  (:documentation "an item that can be killed; if it is dead, it should be removed."))

(when +debug+
  (defmethod (setf dead-p) :after (new-value (killable-item killable-item))
    (declare (ignore new-value))
    (when
        +debug+
      (format t "killing item: ~A~%" killable-item))))

;; so we can "kill" NIL
(defmethod (setf dead-p) (new-value (killable-item list)) 
  ()) 

(defmethod kill ((killable-item killable-item))
  (setf (dead-p killable-item) t))

;; what in the heck does this do? 
;(defmethod kill (item)
;  (setf (dead-p item) t))
