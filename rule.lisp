;;;; Logs extensible (common-lisp based) log/event analysis engine/language
;;;; Copyright (C) 2003-2008 James Earl Prewett

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

(defclass rule (killable-item named-object timeout-object relative-timeout-object environment-object)
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
    (with-slots 
          (name match delete-rule timeout relative-timeout continuep actions environment match-count match-try)
        obj
      (format stream "name: ~A match: ~A delete-rule: ~A timeout: ~A relative timeout: ~A continuep: ~A actions: ~A environment: ~A  match-count: ~A  match-try: ~A"
              name match delete-rule timeout relative-timeout continuep actions environment match-count match-try))))

(defgeneric rule-exceeded-limit-p (rule time)
  (:documentation "see if the given rule has exceeded one of its limits (currently only timeout)"))

(defmethod rule-exceeded-limit-p ((rule rule) time)
  (> time (timeout rule)))


(defgeneric rule-matches-p (rule message environment)
  (:documentation "check to see if a rule matches a message given an environment"))

(defmethod rule-matches-p ((rule rule) (message message) environment)
  (declare (OPTIMIZE SPEED (DEBUG 0) (SAFETY 0)))  
  (with-slots (match) rule
    ;; do bookkeeping
    (when +enable-rule-count+
      (when *count-rules*
        (LoGS-debug "incrementing rule try count~%")
        (incf (match-try rule))))
    (multiple-value-bind (matches match-environment)
        (cond ((functionp match)
               (logs-debug "match: ~A message: ~A environment: ~A~%"
                           match message environment)
               (funcall match message environment))
              (t match))
      (when matches
           ;; bookkeeping ; increment the count since we've matched
        (when +enable-rule-count+
          (when *count-rules*
            (LoGS-debug "incrmenting rule match count~%")
            (incf (match-count rule))))
        (values matches match-environment)))))

(defgeneric run-actions (rule message environment)
  (:documentation "run a rule's actions."))

(defmethod run-actions ((rule rule) (message message) environment)
  (declare (OPTIMIZE SPEED (DEBUG 0) (SAFETY 0)))
  (with-slots (actions) rule
    (when actions
      (logs-debug "actions: ~A~%" actions)
      (mapcar
       #'(lambda (action)
           (LoGS-debug "running action ~A with env ~A and args: ~A~%"
                       action environment message)
           (let ((*environment* environment))
             (funcall action message environment))
           (LoGS-debug "ran action for rule: ~A~%" (name rule)))
       actions))))

;; check-rule should return 2 values, whether the rule matched
;; and whether the you should continue.
(defgeneric check-rule (rule message environment)
  (:documentation "see if the given message matches the rule, if so, run the rule's actions."))

(defmethod check-rule ((rule rule) (message message) inherited-environment)
  (declare (OPTIMIZE SPEED (DEBUG 0) (SAFETY 0)))
  (unless (dead-p rule)
    (with-slots (delete-rule actions environment) rule
      (LoGS-debug "checking rule: ~A~%against message: ~A~% in environment: ~A"
                  (name rule) (message message) inherited-environment)
      (multiple-value-bind (matchp match-environment)
          (rule-matches-p rule message 
                          (append environment inherited-environment))
        (when matchp
          (update-relative-timeout rule)
          (when actions
            (run-actions rule message 
                         (append match-environment environment inherited-environment)))
          (when 
              (and delete-rule 
                   (funcall 
                    delete-rule message 
                    (append match-environment 
                            environment inherited-environment)))
            (setf (dead-p rule) t)
            (dll-delete *ruleset* rule))
          (values matchp match-environment))))))

(defmethod check-limits :around ((rule rule))
  (when (call-next-method)
    (setf (dead-p rule) t)))

(defmethod (setf dead-p) :after (new-value (rule rule))
  (when (eq new-value t)
    (LoGS-debug "killing rule: ~A name: ~A~%" rule (name rule))))

(defmethod display-count ((rule rule) stream)
  (format stream "~A ~A ~A~%" (name rule) (match-count rule) (match-try rule)))

;; not sure I like this name :P
(defun get-LoGS-env-var (var-name environment-list)
  (let ((val 
         (assoc var-name environment-list)))
    (values
     (cadr val)
     (not (null val)))))

(defmacro with-LoGS-env-vars ((&rest vars) environment &body body)
  `(let (,@(mapcar 
            (lambda (var)
              (list var `(get-logs-env-var ',var ,environment)))
            vars))
     ,@body))
