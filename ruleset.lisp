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

;;; ruleset is now the name of a data structure!
;;; that data structure's stuff goes here, move macros, methods
;;; for ruleset creation elsewhere.

(defclass ruleset (rule doubly-linked-list)
  ((elements :initform (make-hash-table :test #'equal) :accessor elements
             :documentation "a hash table to hold all of the rules in this ruleset"))
  (:documentation "A class to store rules."))

(defmethod enqueue :around ((ruleset ruleset) (rule rule))
  (progn
    (setf (gethash (name rule) (elements ruleset)) rule)
    (call-next-method)))

(defmethod dll-insert :around ((ruleset ruleset)
                               neighbor-item 
                               (insert-item rule) &key direction)
  (declare (ignore direction))
  (progn 
    (setf (gethash (name insert-item) (elements ruleset)) insert-item)
    (call-next-method)))

;; find a rule in the ruleset by its name
(defmethod get-rule ((ruleset ruleset) (name t))
  (gethash name (elements ruleset)))

;; check rule for when the rule itself is a ruleset!
(defmethod check-rule ((ruleset ruleset) (message message))
  (with-slots (match no-match delete-rule no-delete-rule actions) 
      ruleset

    (multiple-value-bind (matches sub-matches)
        (if (functionp match)
            (funcall match message)
            match)

      (when delete-rule 
        (and
         (funcall delete-rule message)
         (if no-delete-rule
             (funcall no-delete-rule message)
             t)
         (setf (dead-p rule) t)
         (dll-delete *ruleset* rule)))

      (when matches
        (if (or (not (functionp no-match))
                (not (funcall no-match message)))
            (let ((*ruleset* ruleset))
              (check-rules message *ruleset*)))))))

(defmethod rule-head ((rule rule))
  (dll-insert
   *ruleset*
   (head *ruleset*)
   rule))

(defmethod rule-before ((rule rule))
  (unless *current-rule*
    (error "no current rule"))
  (dll-insert
     *ruleset*
     (or (gethash (data *current-rule*) (list-entries *ruleset*))
         (error "WTF?"))
     rule))

(defmethod rule-after ((rule rule))
  (dll-insert
    *ruleset*
    (gethash (data *current-rule*) (list-entries *ruleset*))
    rule
    :direction :after))

(defmethod rule-tail ((rule rule))
  (dll-insert
   *ruleset*
   (tail *ruleset*)
   rule
   :direction :after))

;; stealing from SEC
(defun Single (match actions)
    (make-instance 'rule
                   :match match
                   :actions actions))

(defmethod dll-delete :after ((ruleset ruleset)
                              (item doubly-linked-list-item))
  (remhash (name (data item)) (elements ruleset)))
