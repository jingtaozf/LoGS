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
  (let ((rule (gethash name (elements ruleset))))
    (if +debug+
        (or rule (format t "can't find rule named: ~A in ruleset: ~A~%" name ruleset))
        rule)))

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
         (setf (dead-p ruleset) t)
         (dll-delete *ruleset* ruleset)))

      (when matches
        (if (or (not (functionp no-match))
                (not (funcall no-match message)))
            (let ((*ruleset* ruleset))
              (check-rules message *ruleset*)))))))

(defgeneric check-rules (message ruleset)
  (:documentation
   "Check-rules checks the given message against 
the given ruleset until it finds a rule that 
both matches and continuep is nil."))

(defmethod check-rules ((message message) (ruleset ruleset))
  (let ((head (head ruleset))
        (*ruleset* ruleset))
    (when
        +debug+
      (format t "checking rules: ~A ~A~%" (name ruleset) (message message)))

    (loop with *current-rule* = head
       and found = ()

       ;; there's a rule to check
       when *current-rule*
       do 
       (let ((data (data *current-rule*)))
         (if (dead-p data)
             (dll-delete ruleset *current-rule*)
             (progn
               (when +debug+ (format t "checking rule~%"))
               (and 
                (multiple-value-bind
                      (matchp bind-list)
                    (check-rule data message)
                  (declare (ignore bind-list))
                  (when +debug+
                    (format t "match is: ~A~%" matchp))
                  (setq found matchp))
                  
                (unless (continuep data)
                  (return t))))))
       (setq *current-rule* (rlink *current-rule*))
        
       ;; there are no (more) rules in this ruleset
       when (not *current-rule*)
       do
       (when +debug+
         (format t "no more rules~%"))
       (return found))))

(defmethod rule-head ((rule rule))
  (dll-insert
   *ruleset*
   (head *ruleset*)
   rule
   :direction :before))

(defmethod rule-before ((rule rule))
  (unless *current-rule*
    (error "no current rule"))
  (dll-insert
   *ruleset*
   (or (gethash (data *current-rule*) (list-entries *ruleset*))
       (error "WTF?"))
   rule
   :direction :before))

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

(defmethod dll-delete :after ((ruleset ruleset)
                              (item doubly-linked-list-item))
  (remhash (name (data item)) (elements ruleset)))
