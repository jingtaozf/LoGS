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

(in-package :LoGS)

(defclass rule (killable-item named-object timeout-object)
  ((match :initarg :match
          :accessor match
          :initform ())
   (no-match :initarg :no-match
             :accessor no-match
             :initform ())
   (delete-rule :initarg :delete-rule
           :accessor delete-rule
           :initform ())
   (no-delete-rule :initarg :no-delete-rule
              :accessor no-delete-rule
              :initform ())
   (continuep :initarg :continuep
              :initform ()
             :accessor continuep)
   (actions :initarg :actions
            :accessor actions
            :initform ()))
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

(defmethod check-rule ((rule rule) (message message))
  (multiple-value-bind (matches sub-matches)
      (rule-matches-p rule message)
    (when *debug* 
      (format t "checking rule: ~A~%" (name rule)))
    (if matches
        (with-slots (delete-rule no-delete-rule actions) 
            rule
          
          (when delete-rule 
            (and
             (funcall delete-rule message)
             (if no-delete-rule
                 (funcall no-delete-rule message)
                 t)
             (setf (dead-p rule) t)
             (dll-delete *ruleset* rule)))
          
          (when actions
            (run-actions rule message matches sub-matches))
          (values matches sub-matches)))))

(defgeneric run-actions (rule message matches sub-matches)
  (:documentation "run a rule's actions."))

(defmethod run-actions ((rule rule) message matches sub-matches)
  (let ((actions (actions rule)))
    (when actions
      (mapcar 
       (lambda (action)
         (declare (function action))
         (progn
           (funcall action message matches sub-matches)))
       actions))))

;; (defmethod (setf timeout) :after (new-value (rule rule))
;;   (progn
;;     (dll-delete *rule-timeout-queue* rule)
;;     (enqueue *rule-timeout-queue* rule)))
