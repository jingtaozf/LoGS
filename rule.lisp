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

(defclass rule (killable-item named-object timeout-object)
  ((match :initarg :match
          :accessor match
          :initform ()
          :type (or null function))
   (no-match :initarg :no-match
             :accessor no-match
             :initform ()
             :type (or null function))
   (delete-rule :initarg :delete-rule
           :accessor delete-rule
           :initform ()
           :type (or null function))
   (no-delete-rule :initarg :no-delete-rule
              :accessor no-delete-rule
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
                :type list))
  (:documentation "Rules associate messages with actions."))

(defgeneric rule-exceeded-limit-p (rule time)
  (:documentation "see if the given rule has exceeded one of its limits (currently only timeout)"))

(defmethod rule-exceeded-limit-p ((rule rule) time)
  (when (timeout rule)
    (> time (timeout rule))))

;; check-rule should return 2 values, whether the rule matched
;; and whether the you should continue.

(defgeneric check-rule (rule message)
  (:documentation "see if the given message matches the rule, if so, run the rule's actions."))

(defmethod rule-matches-p ((rule rule) (message message))
  (with-slots (match no-match)
      rule
    
    (multiple-value-bind (matches sub-matches)
        (cond ((functionp match) (funcall match message))
              (t match))

      (and matches
           (not (cond ((functionp no-match) (funcall no-match message))
                      (t no-match)))
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

;; (defmethod check-rule ((rule rule) (message message))
;;   (unless (dead-p rule)
;;     (multiple-value-bind (matches sub-matches)
;;         (rule-matches-p rule message)
;;       (declare (special matches sub-matches))
;;       (when +debug+ 
;;         (format t "checking rule: ~A~%" (name rule)))
;;       (if matches
;;           (with-slots (delete-rule no-delete-rule actions) 
;;               rule
            
;;             (when actions
;;               (if (listp matches)
;;                   (in-given-environment
;;                    (eval matches)
;;                    (run-actions rule message matches sub-matches))
;;                   (run-actions rule message matches sub-matches)))
            
;;             (when delete-rule 
;;               (and
;;                (funcall delete-rule message)
;;                (if no-delete-rule
;;                    (funcall no-delete-rule message)
;;                    t)
;;                (setf (dead-p rule) t)
;;                (dll-delete *ruleset* rule)))
;;             (values matches sub-matches))))))

(defmethod check-rule ((rule rule) (message message))
  (unless (dead-p rule)
    (when +debug+ 
        (format t "checking rule: ~A~%against message: ~A~%" (name rule) (message message)))
      
      (in-given-environment 
       (environment rule)
       
       (multiple-value-bind (matchp environment)
           (rule-matches-p rule message)

         (when matchp
             (with-slots (delete-rule no-delete-rule actions) 
                 rule
               
               (when actions
                 (run-actions rule message environment))
               
               (when delete-rule 
                 (and
                  (funcall delete-rule message)
                  (if no-delete-rule
                      (funcall no-delete-rule message)
                      t)
                  (setf (dead-p rule) t)
                  (dll-delete *ruleset* rule)))
               
               (values matchp environment)))))))

(defmethod check-rule ((rule rule) (message message))
  (unless (dead-p rule)

    (when +debug+ 
        (format t "checking rule: ~A~%against message: ~A~%" (name rule) (message message)))

    (with-slots (environment) rule
      (in-given-environment 
       environment
       
       (multiple-value-bind (matchp rule-environment)
           (rule-matches-p rule message)
         
           (when matchp

             (with-slots (delete-rule no-delete-rule actions environment) 
                 rule  

               (when actions
                 (run-actions rule message rule-environment))
               (when delete-rule 
                 (and
                  (funcall delete-rule message)
                  (if no-delete-rule
                      (funcall no-delete-rule message)
                      t)
                  (setf (dead-p rule) t)
                  (dll-delete *ruleset* rule)))
               
               (values matchp rule-environment))))))))
  
(defgeneric run-actions (rule message matches)
  (:documentation "run a rule's actions."))

(defmethod run-actions ((rule rule) (message message) matchesQ)
  (let ((actions (actions rule)))
    (when actions
      (mapcar 
       (lambda (action)
         (declare (function action))
         
         (progn
           (when +debug+
             (format t "running action ~A in env ~A with args: ~A~%"
                     action 
                     matchesQ
                     message))
           
           (in-given-environment
            matchesQ
            action
            message)

           (when +debug+ 
             (format t "ran action~%"))))
       actions))))

(defmethod check-limits ((rule rule))
  (and (rule-exceeded-limit-p rule *now*)
       (setf (dead-p rule) t)))

(defmethod (setf dead-p) :after (new-value (rule rule))
  (when +debug+
    (format t "killing rule: ~A name: ~A~%" rule (name rule))))
