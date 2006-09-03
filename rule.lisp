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

(in-package :org.prewett.LoGS)

(defclass rule (killable-item named-object timeout-object relative-timeout-object)
  ((match :initarg :match
          :accessor match
          :initform ()
          :type (or null function))
   (delete-rule :initarg :delete-rule
           :accessor delete-rule
           :initform ()
           :type (or null function))
   (continuep :initarg :continuep
              :initform ()
              :accessor continuep)
   (actions :initarg :actions
            :accessor actions
            :initform ()
            :type list)
   (environment :initarg :environment
                :accessor environment
                :initform ()
                :type list)
   (match-count :accessor match-count
                :initform 0
                :type integer)
   (match-try :accessor match-try
                :initform 0
                :type integer))
  (:documentation "Rules associate messages with actions."))

 (defmethod make-load-form ((self rule) &optional environment)
    (make-load-form-saving-slots self :environment environment))

(defmethod print-object ((obj rule) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (name match delete-rule continuep actions environment match-count match-try) obj
    (format stream "name: ~A match: ~A delete-rule: ~A continuep: ~A actions: ~A environment: ~A  match-count: ~A  match-try: ~A"
            name match delete-rule continuep actions environment match-count match-try))))

(defmacro rule (&rest rest)
  `(make-instance 'rule ,@rest))

(defgeneric rule-exceeded-limit-p (rule time)
  (:documentation "see if the given rule has exceeded one of its limits (currently only timeout)"))

(defmethod rule-exceeded-limit-p ((rule rule) time)
  (with-slots (timeout) rule
    (> time timeout)))

(defmethod rule-matches-p ((rule rule) (message message))
  (with-slots (match)
      rule
    
    ;; do bookkeeping
    (when +enable-rule-count+
      (when *count-rules*
        (LoGS-debug "incrementing rule try count~%")
        (incf (match-try rule))))

    (multiple-value-bind (matches sub-matches)
        (cond ((functionp match) (funcall match message))
              (t match))
      (when matches
           ;; bookkeeping ; increment the count since we've matched
           (or
            (when +enable-rule-count+
              (when *count-rules*
                (LoGS-debug "incrmenting rule match count~%")
                (incf (match-count rule))))
              t)
           (values matches sub-matches)))))

(defmacro in-given-environment (env body &rest args)
  (let* ((vars (gensym))
         (vals (gensym))
         (body-result (gensym))
         (funcall-body (if args 
                           `(funcall ,body-result ,@args)
                           `(funcall ,body-result))))
    `(let ((,vars (mapcar #'car ,env))
           (,vals (mapcar #'cadr ,env)))
       `(declare (special ,@,vars))
       (progv (cons 'env ,vars) (cons ,env ,vals)
         (let ((,body-result ,body))
           (cond ((functionp ,body-result)
                  ,funcall-body)
                 (t
                  ,body-result)))))))

(defgeneric run-actions (rule message environment)
  (:documentation "run a rule's actions."))

(defmethod run-actions ((rule rule) (message message) environment)
  (let ((actions (actions rule)))
    (when actions
      (mapcar 
       (lambda (action)
         (progn
           (LoGS-debug "running action ~A in env ~A with args: ~A~%"
                     action 
                     environment
                     message)

           (in-given-environment
                environment
                action
                message)

           (LoGS-debug "ran action for rule: ~A~%" (name rule))))
       actions))))

;; check-rule should return 2 values, whether the rule matched
;; and whether the you should continue.
(defgeneric check-rule (rule message)
  (:documentation "see if the given message matches the rule, if so, run the rule's actions."))

(defmethod check-rule ((rule rule) (message message))
  (declare (OPTIMIZE (SPEED 3) (DEBUG 0) (SAFETY 0)))  
  (unless (dead-p rule)

    (LoGS-debug "checking rule: ~A~%against message: ~A~%" 
                (name rule) (message message))

    (with-slots (environment) rule
      (in-given-environment 
       environment
       
       (multiple-value-bind (matchp rule-environment)
           (rule-matches-p rule message)
         
         (when matchp
           (update-relative-timeout rule)
           (with-slots (delete-rule environment) 
               rule
             
             (when (actions rule)
               (run-actions rule message (append rule-environment environment)))
             (when delete-rule 
               (when (funcall delete-rule message)
                (setf (dead-p rule) t)
                (dll-delete *ruleset* rule)))
             
             (values matchp rule-environment))))))))

(defmethod check-limits :around ((rule rule))
  (when (call-next-method)
    (setf (dead-p rule) t)))

(defmethod (setf dead-p) :after (new-value (rule rule))
  (when (eq new-value t)
    (LoGS-debug "killing rule: ~A name: ~A~%" rule (name rule))))

(defmethod display-count ((rule rule) stream)
  (format stream "~A ~A ~A~%" (name rule) (match-count rule) (match-try rule)))
