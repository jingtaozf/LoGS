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

;;; collections
;; a class that holds things. 
;; When the underlying array runs out of space, it is dynamically
;; resized.

(defclass limited-collection (collection) 
  ((max-lines :initarg :max-lines
              :initform ()
              :accessor max-lines
              :documentation "An artificial limit on the number of lines this context can hold")))

(defmethod check-limits OR ((limited-collection limited-collection))
           (if +debug+
               (let ((ret (with-slots (ecount max-lines) limited-collection
                            (when (and max-lines ecount)
                              (> ecount max-lines)))))
               (format t "checking limited-collection limits. ret: ~A~%" ret)
               ret)
               (with-slots (ecount max-lines) limited-collection
                 (when (and max-lines ecount)
                   (> ecount max-lines)))))