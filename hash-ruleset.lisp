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

;;; ruleset is now the name of a data structure!
;;; that data structure's stuff goes here, move macros, methods
;;; for ruleset creation elsewhere.

(in-package :org.prewett.LoGS)

(defclass hash-ruleset (ruleset)
  ((table :initform (make-hash-table :test #'equal) :accessor table)
   (keyfunc :initarg :keyfunc :accessor keyfunc))
  (:documentation "A class to store rules."))

(defmethod check-rule ((ruleset hash-ruleset)
                       (message message)
                       environment)
  (format t "checking hash rules~%")
  (check-rules message ruleset environment))

(defmethod check-rules ((message message) (ruleset hash-ruleset) environment)
  (let* ((key (funcall (keyfunc ruleset) message))
         (rule (gethash key (table ruleset))))
    (if rule
        (let ((ret (check-rule rule message environment)))
          (format t "returning: ~A~%" ret)
          (format t "from: ~A ~A~%" rule message)
          ret
          )
        (warn "no such rule: ~A" key))))

(defmethod add-rule ((ruleset hash-ruleset) (rule rule) key)
  (setf (gethash key (table ruleset))
        rule))
