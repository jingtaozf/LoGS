;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2006 James Earl Prewett

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

(defclass ruleset (rule doubly-linked-list)
  ((elements :initform (make-hash-table :test #'equal) :accessor elements
             :documentation "a hash table to hold all of the rules in this ruleset"))
  (:documentation "A class to store rules."))

(defmacro ruleset (&rest rest)
  `(make-instance 'ruleset ,@rest))

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
    (unless rule
      (LoGS-debug "can't find rule named: ~A in ruleset: ~A~%" name ruleset))
    rule))

;; check rule for when the rule itself is a ruleset!
(defmethod check-rule ((ruleset ruleset) (message message))
  (unless (dead-p ruleset)

    (LoGS-debug "checking ruleset: ~A~%against message: ~A~%" 
                (name ruleset) (message message))


    (with-slots (environment) ruleset
        (in-given-environment
         environment

         (multiple-value-bind (matchp rule-environment)
             (rule-matches-p ruleset message)
           (when matchp
             (update-relative-timeout ruleset)

             (with-slots (delete-rule (ruleset-environment environment))
               ruleset
        
               (when delete-rule 
                 (when (funcall delete-rule message)
                   (setf (dead-p ruleset) t)
                   (dll-delete *ruleset* ruleset)))
        
               (let ((*ruleset* ruleset))
                 (in-given-environment
                  (append environment rule-environment ruleset-environment)
                 (check-rules message *ruleset*))))))))))

(defgeneric check-rules (message ruleset)
  (:documentation
   "Check-rules checks the given message against 
the given ruleset until it finds a rule that 
both matches and continuep is nil."))

(defmethod check-rules ((message message) (ruleset ruleset))
  (declare (OPTIMIZE (SPEED 3) (DEBUG 0) (SAFETY 0)))  
  (let ((head (head ruleset))
        (*ruleset* ruleset))
    (LoGS-debug "checking rules: ~A ~A~%" (name ruleset) (message message))
    (let 
        ((didmatch
          
          (loop with *current-rule* = head
             and found = ()
               
             ;; there's a rule to check
             when *current-rule*
             do 
               (let ((data (data *current-rule*)))
                 (if (dead-p data)
                     ;; if the rule is dead, remove it
                     (dll-delete ruleset *current-rule*)
                     ;; else check the rule against the message
                     (progn
                       (LoGS-debug "checking rule~%")
                       (and 
                        (multiple-value-bind
                              (matchp bind-list)
                            (check-rule data message)
                          (declare (ignore bind-list))
                          (LoGS-debug "match is: ~A~%" matchp)
                          (setq found matchp))
                        
                        (unless (continuep data)
                          (return t))))))
               (setq *current-rule* (rlink *current-rule*))
        
             ;; there are no (more) rules in this ruleset
             when (not *current-rule*)
             do
               (LoGS-debug "no more rules~%")
               (return found))))
      (when didmatch
        (LoGS-debug "updating ruleset's relative timeout~%")
        (update-relative-timeout ruleset))
      didmatch)))

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

(defmethod display-count ((ruleset ruleset) stream)
  (progn
    (format stream "~A ~A ~A~%" (name ruleset) (match-count ruleset) (match-try ruleset))
    (loop with *current-rule* = (head ruleset)
       when *current-rule*
       do
         (display-count (data *current-rule*) stream)
         (setq *current-rule* (rlink *current-rule*))

         when (not *current-rule*)
         do
         (return t))))
         
;; so that initializing these can be moved out of LoGS.lisp
(setf *root-ruleset* (make-instance 'ruleset :name 'root-ruleset))
(setf *ruleset* *root-ruleset*) 
